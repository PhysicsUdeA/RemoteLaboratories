program WaveGenerator;

uses
  Forms,
  SC_MainForm in 'SC_MainForm.pas' {Form1},
  FormAbout in 'FormAbout.pas' {Frm_About};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TComPort ver. 2.10 example';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFrm_About, Frm_About);
  Application.Run;
end.
