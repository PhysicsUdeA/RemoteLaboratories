unit SC_MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CPort, CPortCtl, Menus, StrUtils, VclTee.TeeGDIPlus,
  VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart;

type
  TForm1 = class(TForm)
    ComPort: TComPort;
    grap: TMainMenu;
    Configuracion1: TMenuItem;
    Serial1: TMenuItem;
    ButConecct: TButton;
    Memo1: TMemo;
    ButFreq: TButton;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Acercade1: TMenuItem;
    Acercade2: TMenuItem;
    Label6: TLabel;
    EditFreq: TEdit;
    ButGener: TButton;
    Chart1: TChart;
    Series1: TFastLineSeries;
    Timer1: TTimer;
    procedure ComPortOpen(Sender: TObject);
    procedure ComPortClose(Sender: TObject);
    procedure ComPortRxChar(Sender: TObject; Count: Integer);
    procedure Serial1Click(Sender: TObject);
    procedure ButConecctClick(Sender: TObject);
    procedure FormOnClose(Sender: TObject; var Action: TCloseAction);
    procedure ButFreqClick(Sender: TObject);
    procedure Acercade2Click(Sender: TObject);
    procedure ButGenerClick(Sender: TObject);
    procedure EditFreqKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
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

  value: Array[0..100] of Double;
  time: Double;

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
      str := '1,0;';
      ComPort.WriteStr(str);
      Sleep(2000);
      ComPort.Close;

    end
    else
    begin
      str := '1,0;';
      ComPort.Open;
      Sleep(2000);
      ComPort.WriteStr(str);

    end;

  except

    application.MessageBox(pchar('El Puerto no ha sido configurado'),
      pchar('Error'), (MB_OK + MB_ICONSTOP));

  end;

end;

procedure TForm1.ButFreqClick(Sender: TObject);
var
  editText, str: String;
begin

  editText := EditFreq.Text;
  if (editText = '') then
    editText := '0';

  str := '3,' + editText + ';';
  Label1.Caption := str;
  ComPort.WriteStr(str);

end;

procedure TForm1.ButGenerClick(Sender: TObject);

var
  str: String;
begin

  if (freqState = 0) then
  begin
    str := '2,0;';
    ComPort.WriteStr(str);
    Label1.Caption := str;
  end
  else if (freqState = 1) then
  begin
    str := '1,0;';
    ComPort.WriteStr(str);
    Label1.Caption := str;
  end;

end;

procedure TForm1.ComPortOpen(Sender: TObject);
begin
  ButConecct.Caption := 'Desconectar';

  ButFreq.Enabled := False;
  EditFreq.Enabled := False;
  ButGener.Enabled := True;
end;

procedure TForm1.ComPortClose(Sender: TObject);
begin
  if ButConecct <> nil then
    ButConecct.Caption := 'Conectar';

  ButFreq.Enabled := False;
  EditFreq.Enabled := False;
  ButGener.Enabled := False;
end;

procedure TForm1.ComPortRxChar(Sender: TObject; Count: Integer);
var
  str, min, sec, compose: String;
  List: TArray<String>;
  command: Integer;

begin

  ComPort.ReadStr(str, Count);

  dataRead := dataRead + str;

  if ContainsText(dataRead, ';') then
  begin
    List := dataRead.Split([',', ';'], 2);

    if Length(List) = 2 then
    begin
      command := StrToInt(List[0]);
      freqValue := StrToFloat(List[1]);

      if (command = 3) then
      begin
        Label1.Caption := FloatToStr(freqValue);
        compose := 'Frecuencia = ' + FloatToStr(freqValue) + ' Hz';

        // escribir datos en el memo

        Memo1.Text := Memo1.Text + compose + #$D#$A;
        SendMessage(Memo1.Handle, WM_VSCROLL, SB_BOTTOM, 0);
      end
      else if (command = 2) then
      begin
        freqState := 1;
        ButGener.Caption := 'Apagar generador';
        EditFreq.Enabled := True;
        ButFreq.Enabled := True;
      end
      else if (command = 1) then
      begin
        freqState := 0;
        ButGener.Caption := 'Encender generador';
        EditFreq.Enabled := False;
        ButFreq.Enabled := False;
      end
      else if (command = 0) then
      begin
        freqState := 0;
        compose := 'Sistema conectado';
        ButGener.Caption := 'Encender generador';

        // escribir datos en el memo

        Memo1.Text := Memo1.Text + compose + #$D#$A#$D#$A;
        SendMessage(Memo1.Handle, WM_VSCROLL, SB_BOTTOM, 0);
      end;

    end;

    dataRead := '';

  end;

end;

procedure TForm1.EditFreqKeyPress(Sender: TObject; var Key: Char);
begin

  if Ord(Key) = VK_RETURN then
    ButFreqClick(ButFreq);

  (* only return value if numeric *)
  if not CharInSet(Key, ['0' .. '9', '.', #8]) then
    Key := #0;

end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Series1.Clear;

  for i := 0 to 100 do
  begin
    value[i] := Sin(time);
    time := time + 0.1;
  end;

  Series1.AddArray(value);

end;

procedure TForm1.FormOnClose(Sender: TObject; var Action: TCloseAction);
begin

  if ComPort.Connected then
  begin
    try
      ComPort.WriteStr('1,0;');
      Sleep(50);
      ComPort.Close;
    finally

    end;

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

  for i := 0 to 99 do
  begin
    value[i] := value[i+1];

  end;

  time := time + 0.1;
  value[100] := Sin(time);

  Series1.AddArray(value);

end;

end.
