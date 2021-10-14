unit BatchEditF;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TBatchEditForm = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    FOp: Byte;
    FValue: Variant;
    FIdxStart: Integer;
    FIdxEnd: Integer;
    FIsText: Boolean;
    { Public declarations }
  end;

var
  BatchEditForm: TBatchEditForm;

implementation

{$R *.dfm}

procedure TBatchEditForm.Button1Click(Sender: TObject);
var
  i, ValStart: Integer;
  Str: String;
  IsFloat: Boolean;
  Err: Boolean;
  FloatVal: Single;
  ValInt: Integer;
begin
  Str := Edit1.Text;
  ValStart := 0;
  IsFloat := False;
  Err := False;

  if not FIsText then
  begin



  for i := 1 to Length(Str) do
  begin
    if (i = 1) then
    begin
      if Str[i] = '+' then
        FOp := 1
      else if Str[i] = '-' then
        FOp := 2
      else if Str[i] = '*' then
        FOp := 3
      else if Str[i] = '/' then
        FOp := 4
      else
        FOp := 0;
      if FOp <> 0 then continue;
    end;
    if ((Str[i] >= '0') and (Str[i] <= '9')) or (Str[i] = '.') then
    begin
      if (Str[i] <> '.') and (ValStart = 0) then
        ValStart := i;

      if (Str[i] = '.') and (ValStart <> 0) then
      begin
        IsFloat := True;
        Continue;
      end;


      if i = Length(Str) then
        Str := Copy(Str, ValStart, i - (ValStart - 1));
    end
    else
    begin
      Err := True;
      break;
    end;
  end;

  if Err then
  begin
    MessageDlg('Invalid expression.', mtError, [mbOK], 0);
    Exit;
  end;

  if IsFloat then
  begin
    if TryStrToFloat(Str, FloatVal)  then
      FValue := FloatVal
    else
      Err := True;
  end
  else
  begin
    if TryStrToInt(Str, ValInt)  then
      FValue := ValInt
    else
      Err := True;
  end;

  end
  else
  begin
    FValue := Str;
    FOp := 0;
  end;


   if TryStrToInt(Edit2.Text, ValInt)  then
     FIdxStart := ValInt
   else
     Err := True;

   if TryStrToInt(Edit3.Text, ValInt)  then
     FIdxEnd := ValInt
   else
     Err := True;

  if Err then
    MessageDlg('Invalid numeric value.', mtError, [mbOK], 0)
  else
    Self.ModalResult := mrOk;
end;

end.
