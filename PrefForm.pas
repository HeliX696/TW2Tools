unit PrefForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TForm3 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Panel1: TPanel;
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    { Private declarations }
  public
     FValue: Integer;
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if (Key < '0') or (Key > '9') then
    Key := #0;
end;

procedure TForm3.FormHide(Sender: TObject);
begin
  Edit1.Text := IntToStr(FValue);
end;

procedure TForm3.FormShow(Sender: TObject);
begin
  if not TryStrToInt(Edit1.Text, FValue) then
    FValue := 30;
  if FValue > 300 then FValue := 300;
end;

end.
