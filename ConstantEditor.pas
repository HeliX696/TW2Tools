unit ConstantEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Grids, ExtCtrls, ToolWin, Tabs, StdCtrls, Buttons, ImgList,
  System.ImageList;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    StringGrid1: TStringGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    TabSet1: TTabSet;
    Panel2: TPanel;
    Edit1: TEdit;
    BitBtn1: TBitBtn;
    ImageList1: TImageList;
    CheckBox1: TCheckBox;
    procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure TabSet1Change(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure StringGrid1GetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure ToolButton1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure StringGrid1KeyPress(Sender: TObject; var Key: Char);
  private
    FConstNameList: TStringList;
    FDefinesStream: TMemoryStream;
    FRefObjList: TStringList;
    FOldText: String;
    FACol: Integer;
    FARow: Integer;
    FLastFound: Integer;
    FIsEditing: Boolean;
    FEditText: String;
    { Private declarations }
  public
    procedure SetConstantsList(ConstNameList: TStringList; DefinesStream: TMemoryStream; ARefObjList: TStringList);
    procedure FillGrid();
    procedure FillDefineStringGrid();
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses MainForm;
{$R *.dfm}

procedure TForm2.FillGrid;
var
  Idx: Integer;
begin
  FACol := 0;
  FARow := 0;
  FLastFound := 1;
  StringGrid1.Cells[0, 0] := 'Index';
  StringGrid1.Cells[1, 0] := 'Name';
  StringGrid1.RowCount := 2;
  if (FConstNameList = nil) or (FConstNameList.Count = 0) then Exit;
  StringGrid1.ColCount := 2;
  StringGrid1.RowCount := FConstNameList.Count + 1;
  for Idx := 0 to FConstNameList.Count - 1 do
  begin
    StringGrid1.Cells[0, Idx + 1] := IntToStr(Idx);
    StringGrid1.Cells[1, Idx + 1] := FConstNameList.Strings[Idx];
  end;

end;

procedure TForm2.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = VK_F3 then
    BitBtn1Click(Self);
end;

procedure TForm2.BitBtn1Click(Sender: TObject);
var
  I: Integer;
  s1, s2 : String;
begin
  if FLastFound >= StringGrid1.RowCount - 1  then FLastFound := 1;
  for I := FLastFound to StringGrid1.RowCount - 1 do
  begin
    if CheckBox1.Checked then
    begin
      s1 := Edit1.Text;
      s2 := StringGrid1.Cells[FACol, I];
    end
    else
    begin
      s1 := UpperCase(Edit1.Text);
      s2 := UpperCase(StringGrid1.Cells[FACol, I]);
    end;
    if Pos(s1, s2) > 0 then
    begin
      StringGrid1.Row := i;
      FLastFound := I + 1;
      Exit;
    end;
  end;
  FLastFound := 1;
end;

procedure TForm2.FillDefineStringGrid;
var
//  BuffPtr: PByte;
  Idx: Integer;
//  Str: AnsiString;
begin
    FACol := 0;
    FARow := 0;
    FLastFound := 1;
    StringGrid1.Cells[0, 0] := 'Index';
    StringGrid1.Cells[1, 0] := 'String';
    StringGrid1.RowCount := FRefObjList.Count + 1;
    StringGrid1.ColCount := 2;
    for Idx := 0 to FRefObjList.Count - 1 do
    begin
      StringGrid1.Cells[0, Idx + 1] := IntToStr(FConstNameList.Count + Idx);
      StringGrid1.Cells[1, Idx + 1] := FRefObjList.Strings[Idx];
    end;


//    if FDefinesStream = nil then Exit;
//    { parse defines }
//    BuffPtr := FDefinesStream.Memory;
//    Idx := 0;
//    ItemIdx := 0;
//    PrevIdx := 0;
//    StringGrid1.ColCount := 2;
//    StringGrid1.ColWidths[1] := 200;
//    while (Idx < FDefinesStream.Size) do
//    begin
//      if BuffPtr[Idx] = 0 then
//      begin
//       if PrevIdx <> 0 then
//       begin
//        SetLength(Str, Idx - PrevIdx);
//        FDefinesStream.Position := PrevIdx;
//        FDefinesStream.Read(Str[1], Idx - PrevIdx);
////        Item.SubItems.Add(Str);
//        StringGrid1.RowCount := ItemIdx + 1;
//        StringGrid1.Cells[0, ItemIdx] := IntToStr(PrevIdx);
//        StringGrid1.Cells[1, ItemIdx] := Str;
//       end;
//        Inc(ItemIdx);
//
//        PrevIdx := Idx + 1;
//      end;
//      Inc(Idx);
//    end;
end;

procedure TForm2.SetConstantsList(ConstNameList: TStringList;
  DefinesStream: TMemoryStream; ARefObjList: TStringList);
begin
    FConstNameList := ConstNameList;
    FDefinesStream := DefinesStream;
    FRefObjList := ARefObjList;
    FillDefineStringGrid ;
    //FillGrid;
end;

procedure TForm2.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
if ARow = 0 then
  StringGrid1.Canvas.Font.Color := clWhite
else
  StringGrid1.Canvas.Font.Color := clBlack;
end;

procedure TForm2.StringGrid1GetEditText(Sender: TObject; ACol, ARow: Integer;
  var Value: string);
begin
 FOldText := Value;
 FIsEditing := True;
end;

procedure TForm2.StringGrid1KeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Char(VK_RETURN))  and  FIsEditing then
  begin
     FIsEditing := False;
     if TabSet1.TabIndex <> 0 then
       StringGrid1.Cells[FACol, FARow] := FOldText
     else if FACol = 1 then
    begin
      StringGrid1.Cells[FACol, FARow] := FEditText;
      FRefObjList.Strings[FARow - 1] := FEditText;
      TParamItem(FRefObjList.Objects[FARow - 1]).FParamName.ParamData[0] := FEditText;
    end;
  end;
end;

procedure TForm2.StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  FACol := ACol;
  FARow := ARow;
  FIsEditing := False;
end;

procedure TForm2.StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin
  FIsEditing := True;
  FACol := ACol;
  FARow := ARow;
  FEditText := Value;
end;

procedure TForm2.TabSet1Change(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin

ToolButton1.Enabled := (NewTab = 0);
if NewTab = 0 then
  FillDefineStringGrid
else
  FillGrid;
end;

procedure TForm2.ToolButton1Click(Sender: TObject);
var
  lItem: TParamItem;
  AStr: AnsiString;
begin
  AStr := AnsiString(InputBox('New constant', 'Enter a new constant string(do it correct, it is not editable) :', ''));
  if AStr = '' then Exit;
  lItem := TParamItem.Create;
  lItem.FParamName.ParamData[0] := AStr;
  lItem.FIndex := FRefObjList.Count;
  lItem.FIsImported := True;
  FRefObjList.AddObject(String(AStr), lItem);
  FillDefineStringGrid;
end;

end.
