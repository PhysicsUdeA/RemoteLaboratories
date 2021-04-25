program SteamControl;

uses
  Forms,
  SC_MainForm in 'SC_MainForm.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TComPort ver. 2.10 example';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
