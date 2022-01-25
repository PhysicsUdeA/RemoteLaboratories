unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  counter: Integer;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin

  counter:= counter + 1;
  label1.Caption:= 'numero = ' + counter.ToString();

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  counter:= 0;
  label1.Caption:= 'numero = ' + counter.ToString();
end;

end.
