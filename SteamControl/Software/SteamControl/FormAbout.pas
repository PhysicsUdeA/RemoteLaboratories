unit FormAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFrm_About = class(TForm)
    Button1: TButton;
    StaticText1: TStaticText;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Image2: TImage;
    Label9: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    procedure Button1Click(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Frm_About: TFrm_About;

implementation

{$R *.dfm}

procedure TFrm_About.Button1Click(Sender: TObject);
begin
  Self.Close;
end;

end.
