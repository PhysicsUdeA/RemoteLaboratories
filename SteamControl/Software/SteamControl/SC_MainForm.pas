unit SC_MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CPort, CPortCtl, Menus;

type
  TForm1 = class(TForm)
    ComPort: TComPort;
    MainMenu1: TMainMenu;
    Configuracion1: TMenuItem;
    Serial1: TMenuItem;
    ButConecct: TButton;
    But_On: TButton;
    But_Off: TButton;
    Memo1: TMemo;
    procedure ComPortOpen(Sender: TObject);
    procedure ComPortClose(Sender: TObject);
    procedure ComPortRxChar(Sender: TObject; Count: Integer);
    procedure Serial1Click(Sender: TObject);
    procedure ButConecctClick(Sender: TObject);
    procedure But_OnClick(Sender: TObject);
    procedure But_OffClick(Sender: TObject);
    procedure FormOnClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.ButConecctClick(Sender: TObject);
begin

  try
    if (ComPort.Connected) then
    begin
      ComPort.Close;

    end
    else
    begin
      ComPort.Open;

    end;

  except

    application.MessageBox(pchar('El Puerto no ha sido configurado'),
      pchar('Error'), (MB_OK + MB_ICONSTOP));

  end;

end;

procedure TForm1.But_OffClick(Sender: TObject);
var
  Str: String;
begin
  Str := '0';
  ComPort.WriteStr(Str);
end;

procedure TForm1.But_OnClick(Sender: TObject);
var
  Str: String;
begin
  Str := '1';
  ComPort.WriteStr(Str);
end;

procedure TForm1.ComPortOpen(Sender: TObject);
begin
  ButConecct.Caption := 'Close';
end;

procedure TForm1.ComPortClose(Sender: TObject);
begin
  if ButConecct <> nil then
    ButConecct.Caption := 'Open';
end;

procedure TForm1.ComPortRxChar(Sender: TObject; Count: Integer);
var
  Str: String;
begin
  ComPort.ReadStr(Str, Count);
  Memo1.Text := Memo1.Text + Str;
end;

procedure TForm1.FormOnClose(Sender: TObject; var Action: TCloseAction);
begin
  ComPort.Close;
end;

procedure TForm1.Serial1Click(Sender: TObject);
begin
  ComPort.ShowSetupDialog;
end;

end.
