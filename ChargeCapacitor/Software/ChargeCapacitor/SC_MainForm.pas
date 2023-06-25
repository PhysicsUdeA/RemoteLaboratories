unit SC_MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CPort, CPortCtl, Menus, StrUtils, VclTee.TeeGDIPlus,
  VclTee.TeEngine, VclTee.Series, VclTee.TeeProcs, VclTee.Chart,
  Vcl.Imaging.pngimage;

type
  TForm1 = class(TForm)
    ComPort: TComPort;
    grap: TMainMenu;
    Configuracion1: TMenuItem;
    Serial1: TMenuItem;
    ButConecct: TButton;
    Memo1: TMemo;
    GroupBox1: TGroupBox;
    Acercade1: TMenuItem;
    Acercade2: TMenuItem;
    Label6: TLabel;
    ButGener: TButton;
    Chart1: TChart;
    Series1: TFastLineSeries;
    Timer1: TTimer;
    ComboCapacitor: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    ComboPeriod: TComboBox;
    Label4: TLabel;
    ComboSample: TComboBox;
    ButtonSave: TButton;
    ButPause: TButton;
    GroupBox2: TGroupBox;
    Image1: TImage;
    AssistantMode: TMenuItem;
    procedure ComPortOpen(Sender: TObject);
    procedure ComPortClose(Sender: TObject);
    procedure ComPortRxChar(Sender: TObject; Count: Integer);
    procedure Serial1Click(Sender: TObject);
    procedure ButConecctClick(Sender: TObject);
    procedure FormOnClose(Sender: TObject; var Action: TCloseAction);
    procedure Acercade2Click(Sender: TObject);
    procedure ButGenerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButPauseClick(Sender: TObject);
    procedure ClearComPortInputBuffer(Port: TComPort);
    procedure AssistantModeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  freqValue: Double;
  dataRead: String;
  freqState: Integer;

  value, time, noTime: Array [0 .. 100] of Double;

  Capacitors: Array [0 .. 2] of String;
  Periods: Array [0 .. 8] of String;
  Samples: Array [0 .. 5] of String;

implementation

uses Math, FormAbout;

{$R *.DFM}

procedure TForm1.Acercade2Click(Sender: TObject);
begin
  Frm_About.showmodal;
end;

procedure TForm1.ButConecctClick(Sender: TObject);
var
  str: String;
begin

  try
    if (ComPort.Connected) then
    begin
      str := '0,8000,50;';
      ComPort.WriteStr(str);
      Sleep(1500);
      ComPort.Close;

    end
    else
    begin
      str := '0,8000,50;';
      ComPort.Open;
      Sleep(1500);
      ComPort.WriteStr(str);

    end;

  except

    application.MessageBox(pchar('El Puerto no ha sido configurado'),
      pchar('Error'), (MB_OK + MB_ICONSTOP));

  end;

end;

procedure TForm1.ButGenerClick(Sender: TObject);

var
  str: String;
  capacitor, period, sample: String;
  i: Integer;
begin

  capacitor := Capacitors[ComboCapacitor.ItemIndex];
  period := Periods[ComboPeriod.ItemIndex];
  sample := Samples[ComboSample.ItemIndex];

  // configuracion de la escala de tiempo
  for i := 0 to 100 do
  begin
    time[i] := StrToInt(sample) * i / 1000;
    noTime[i] := i;
  end;


  // Envio de dato para configuracion de frecuencias

  str := capacitor + ',' + period + ',' + sample + ';';
  ComPort.WriteStr(str);

  if AssistantMode.Checked = True then
  begin
    Chart1.Axes.Bottom.Maximum := time[100];
    Chart1.Axes.Left.Maximum := 5.0;
    Chart1.Axes.Left.Title.Caption := 'Voltaje (V)';
    Chart1.Axes.Bottom.Title.Caption := 'Tiempo (s)';
  end
  else
  begin
    Chart1.Axes.Bottom.Maximum := noTime[100];
    Chart1.Axes.Left.Maximum := 1050;
    Chart1.Axes.Left.Title.Caption := 'Valor ADC';
    Chart1.Axes.Bottom.Title.Caption := 'Muestras';
  end;

  if capacitor = '1' then
  begin
    Series1.Color := RGB(0, 255, 0);
  end
  else if capacitor = '2' then
  begin
    Series1.Color := RGB(36, 201, 219);
  end
  else if capacitor = '3' then
  begin
    Series1.Color := RGB(240, 15, 32);
  end;

end;

procedure TForm1.ButPauseClick(Sender: TObject);
var
  str: String;
begin
  str := '0,8000,50;';
  ComPort.WriteStr(str);
end;

procedure TForm1.ButtonSaveClick(Sender: TObject);
var
  csvFile: TStringList;
  saveDialog: TSaveDialog;
begin
  saveDialog := TSaveDialog.Create(nil);
  try
    // Configurar opciones del diálogo de guardado
    saveDialog.FileName := 'Datos.csv';
    saveDialog.Filter := 'Archivo CSV (*.csv)|*.csv';

    // Desplegar ventana de diálogo y guardar archivo si el usuario hace clic en "Guardar"
    if saveDialog.Execute then
    begin
      csvFile := TStringList.Create;
      try
        // Agregar cabecera de columnas al archivo CSV
        if AssistantMode.Checked = True then
        begin
          csvFile.Add('Tiempos,Voltajes');
        end
        else
        begin
          csvFile.Add('Muestras,ADC_Val');
        end;

        // Agregar contenido del TMemo al archivo CSV
        csvFile.AddStrings(Memo1.Lines);

        // Guardar archivo CSV
        csvFile.SaveToFile(saveDialog.FileName);
      finally
        csvFile.Free;
      end;

      ShowMessage('Archivo CSV guardado exitosamente.');
    end;
  finally
    saveDialog.Free;
  end;

end;

procedure TForm1.ComPortOpen(Sender: TObject);

begin
  ButConecct.Caption := 'Desconectar';

  ButGener.Enabled := True;
  ButtonSave.Enabled := True;
  ButPause.Enabled := True;

  ClearComPortInputBuffer(ComPort);

end;

procedure TForm1.ComPortClose(Sender: TObject);
begin
  if ButConecct <> nil then
    ButConecct.Caption := 'Conectar';

  ButGener.Enabled := False;
  ButPause.Enabled := False;
end;

procedure TForm1.ClearComPortInputBuffer(Port: TComPort);
var
  Buffer: array [0 .. 255] of AnsiChar;
  BytesRead: Integer;
begin
  // clear the input buffer
  while Port.InputCount > 0 do
  begin
    BytesRead := Port.Read(Buffer, SizeOf(Buffer));
  end;
end;

procedure TForm1.ComPortRxChar(Sender: TObject; Count: Integer);
var
  str, min, sec, compose, payload: String;
  // List: TArray<String>;
  ListDiv: Array [0 .. 1] of String;
  command, i: Integer;
  voltage: Double;

  S, LeftPart, RightPart: string;
  DelimPos: Integer;

begin

  ComPort.ReadStr(str, Count);

  dataRead := dataRead + str;
  while ContainsText(dataRead, ',') do
  begin

    DelimPos := Pos(',', dataRead); // busca la primera ocurrencia del token ' '
    if DelimPos > 0 then
    begin
      ListDiv[0] := Copy(dataRead, 1, DelimPos - 1);
      // divide la cadena en dos partes
      ListDiv[1] := Copy(dataRead, DelimPos + 1, Length(dataRead) - DelimPos);

    end;

    // List := dataRead.Split([',']);

    try
      if ListDiv[0] = '' then
      begin
        ListDiv[0] := '0';
      end;

      command := StrToInt(ListDiv[0]);

      if command > 1024 then
      begin
        command := 1023;
      end;

      if AssistantMode.Checked = True then
      begin
        voltage := RoundTo(command * 5.0 / 1024, -2);
      end
      else
      begin
        voltage := command;
      end;

      for i := 1 to 100 do
      begin
        value[i - 1] := value[i];
      end;
      value[100] := voltage;

    finally
    end;

    dataRead := ListDiv[1];

  end;

  payload := '';
  for i := 0 to 100 do
  begin

    if AssistantMode.Checked = True then
    begin
      payload := payload + FloatToStr(time[i]) + ',' + FloatToStr(value[i]
        ) + #$D#$A;
    end
    else
    begin
      payload := payload + FloatToStr(noTime[i]) + ',' + FloatToStr(value[i]
        ) + #$D#$A;
    end;
  end;

  Memo1.Text := payload;

end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin

  // definicion de los capacitores
  Capacitors[0] := '1';
  Capacitors[1] := '2';
  Capacitors[2] := '3';

  // definicion de periodos
  Periods[0] := '200';
  Periods[1] := '500';
  Periods[2] := '1000';
  Periods[3] := '2000';
  Periods[4] := '4000';
  Periods[5] := '5000';
  Periods[6] := '6000';
  Periods[7] := '8000';

  // definicion de muestreo
  Samples[0] := '10';
  Samples[1] := '20';
  Samples[2] := '40';
  Samples[3] := '50';
  Samples[4] := '100';
  Samples[5] := '200';

  Series1.Clear;

  Series1.AddArray(value);

end;

procedure TForm1.FormOnClose(Sender: TObject; var Action: TCloseAction);
begin

  if ComPort.Connected then
  begin
    try
      ComPort.WriteStr('0,8000,20;');
      Sleep(50);
      ComPort.Close;
    finally

    end;

  end;
end;

procedure TForm1.AssistantModeClick(Sender: TObject);
begin
  if AssistantMode.Checked = False then
  begin
    AssistantMode.Checked := True;
    Chart1.Axes.Bottom.Maximum := time[100];
    Chart1.Axes.Left.Maximum := 5.0;
    Chart1.Axes.Left.Title.Caption := 'Voltaje (V)';
    Chart1.Axes.Bottom.Title.Caption := 'Tiempo (s)';
  end
  else
  begin
    AssistantMode.Checked := False;
    Chart1.Axes.Bottom.Maximum := noTime[100];
    Chart1.Axes.Left.Maximum := 1050;
    Chart1.Axes.Left.Title.Caption := 'Valor ADC';
    Chart1.Axes.Bottom.Title.Caption := 'Muestras';
  end;

end;

procedure TForm1.Serial1Click(Sender: TObject);
begin
  ComPort.ShowSetupDialog;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  i: Integer;
begin

  Series1.Clear;

  if AssistantMode.Checked = True then
  begin
    Series1.AddArray(time, value);
  end
  else
  begin
    Series1.AddArray(noTime, value);
  end;

end;

end.
