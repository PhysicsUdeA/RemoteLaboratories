unit SC_MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CPort, CPortCtl, Menus, StrUtils;

type
  TForm1 = class(TForm)
    ComPort: TComPort;
    grap: TMainMenu;
    Configuracion1: TMenuItem;
    Serial1: TMenuItem;
    ButConecct: TButton;
    But_Steam: TButton;
    But_Laser: TButton;
    Memo1: TMemo;
    ComboTime: TComboBox;
    ComboTemp: TComboBox;
    ButTime: TButton;
    Label1: TLabel;
    ButTemp: TButton;
    ComboSensor: TComboBox;
    ButSensor: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LabelSteam: TLabel;
    LabelLaser: TLabel;
    LabelTemp: TLabel;
    LabelTime: TLabel;
    Acercade1: TMenuItem;
    Acercade2: TMenuItem;
    Label6: TLabel;
    procedure ComPortOpen(Sender: TObject);
    procedure ComPortClose(Sender: TObject);
    procedure ComPortRxChar(Sender: TObject; Count: Integer);
    procedure Serial1Click(Sender: TObject);
    procedure ButConecctClick(Sender: TObject);
    procedure FormOnClose(Sender: TObject; var Action: TCloseAction);
    procedure ButTimeClick(Sender: TObject);
    procedure ButTempClick(Sender: TObject);
    procedure ButSensorClick(Sender: TObject);
    procedure But_SteamClick(Sender: TObject);
    procedure But_LaserClick(Sender: TObject);
    procedure Acercade2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  steamState, laserState, minute, second, firstRead: Integer;
  temperatura: Double;
  dataRead: String;

const
  TimeArray: array [1 .. 20] of string = ('001', '002', '003', '004', '005',
    '006', '007', '008', '009', '010', '011', '012', '013', '014', '015', '016',
    '017', '018', '019', '020');

  TempArray: array [1 .. 18] of string = ('025', '030', '035', '040', '045',
    '050', '055', '060', '065', '070', '075', '080', '085', '090', '095', '100',
    '105', '110');

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
      str := '6,000;';
      ComPort.WriteStr(str);
      Sleep(2000);
      ComPort.Close;

    end
    else
    begin
      str := '6,001;';
      ComPort.Open;
      Sleep(2000);
      ComPort.WriteStr(str);

    end;

  except

    application.MessageBox(pchar('El Puerto no ha sido configurado'),
      pchar('Error'), (MB_OK + MB_ICONSTOP));

  end;

end;

procedure TForm1.ButSensorClick(Sender: TObject);
var
  str: String;
begin

  if (ComboSensor.ItemIndex = 0) then
  begin
    str := '5,001;'
  end;
  if (ComboSensor.ItemIndex = 1) then
  begin
    str := '5,000;'
  end;
  Label1.Caption := str;
  ComPort.WriteStr(str);

end;

procedure TForm1.ButTempClick(Sender: TObject);
var
  str: String;
  value: Integer;
begin

  value := ComboTemp.ItemIndex + 1;
  str := '4,' + TempArray[value] + ';';
  Label1.Caption := str;
  ComPort.WriteStr(str);

end;

procedure TForm1.ButTimeClick(Sender: TObject);
var
  str: String;
  value: Integer;
begin

  value := ComboTime.ItemIndex + 1;
  str := '3,' + TimeArray[value] + ';';
  Label1.Caption := str;
  ComPort.WriteStr(str);

end;

procedure TForm1.But_LaserClick(Sender: TObject);
var
  str: String;
begin

  if (laserState = 0) then
  begin
    str := '2,002;';
    ComPort.WriteStr(str);
    Label1.Caption := str;
  end
  else if (laserState = 1) then
  begin
    str := '1,002;';
    ComPort.WriteStr(str);
    Label1.Caption := str;
  end;

end;

procedure TForm1.But_SteamClick(Sender: TObject);
var
  str: String;
begin

  if (steamState = 0) then
  begin
    str := '2,001;';
    ComPort.WriteStr(str);
    Label1.Caption := str;
  end
  else if (steamState = 1) then
  begin
    str := '1,001;';
    ComPort.WriteStr(str);
    Label1.Caption := str;
  end;

end;

procedure TForm1.ComPortOpen(Sender: TObject);
begin
  ButConecct.Caption := 'Desconectar';

  But_Steam.Enabled := True;
  But_Laser.Enabled := True;
  ButTemp.Enabled := True;
  ButSensor.Enabled := True;
  ButTime.Enabled := True;
end;

procedure TForm1.ComPortClose(Sender: TObject);
begin
  if ButConecct <> nil then
    ButConecct.Caption := 'Conectar';

  But_Steam.Enabled := False;
  But_Laser.Enabled := False;
  ButTemp.Enabled := False;
  ButSensor.Enabled := False;
  ButTime.Enabled := False;
end;

procedure TForm1.ComPortRxChar(Sender: TObject; Count: Integer);
var
  str, min, sec, compose: String;
  List: TArray<String>;

begin

  ComPort.ReadStr(str, Count);

  dataRead := dataRead + str;

  if ContainsText(dataRead, ';') then
  begin
    List := dataRead.Split([',', ';'], 8);

    if Length(List) = 8 then
    begin
      steamState := StrToInt(List[0]);
      laserState := StrToInt(List[1]);
      temperatura := StrToFloat(List[4]);
      minute := StrToInt(List[5]);
      second := StrToInt(List[6]);

      if (steamState = 1) then
      begin
        LabelSteam.Caption := 'Encendido';
        But_Steam.Caption := 'Apagar Vapor';
      end
      else
      begin
        LabelSteam.Caption := 'Apagado';
        But_Steam.Caption := 'Encender Vapor';
      end;

      if (laserState = 1) then
      begin
        LabelLaser.Caption := 'Encendido';
        But_Laser.Caption := 'Apagar Canal 2';
      end
      else
      begin
        LabelLaser.Caption := 'Apagado';
        But_Laser.Caption := 'Encender Canal 2';
      end;

      LabelTemp.Caption := FloatToStr(temperatura);

      if (minute < 10) then
        min := '0' + IntToStr(minute)
      else
        min := IntToStr(minute);

      if (second < 10) then
        sec := '0' + IntToStr(second)
      else
        sec := IntToStr(second);

      LabelTime.Caption := min + ':' + sec;

      // escribir datos en el memo
      compose := 'Temperatura = ' + FloatToStr(temperatura) + ', Tiempo = ' +
        min + ':' + sec + #$D#$A;

      Memo1.Text := Memo1.Text + compose;
      SendMessage(Memo1.Handle, WM_VSCROLL, SB_BOTTOM, 0);

    end;

    dataRead := '';

  end;

end;

procedure TForm1.FormOnClose(Sender: TObject; var Action: TCloseAction);
begin

  if ComPort.Connected then
  begin
    try
      ComPort.WriteStr('1,001;');
      Sleep(50);
      ComPort.WriteStr('1,002;');
      Sleep(50);
      ComPort.WriteStr('6,000;');
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

end.
