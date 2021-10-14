unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ComCtrls, ToolWin, StdCtrls, AnsiStrings, ExtCtrls, ValEdit,
  Menus, ImgList, ConstantEditor, Buttons, IniFiles, PrefForm, BatchEditF,
  System.ImageList;

type
  TCustomParam = class(TObject)
  private
    function GetParamData(Index: Integer): Variant;  virtual;
    procedure SetParamData(Index: Integer; const Value: Variant); virtual;
  public
    function Count: Integer; virtual;
    procedure WriteToStream(Stream: TStream); virtual;
    procedure ReadFromStream(Stream: TStream); virtual;
    procedure Clear; virtual;
    function IsAddrSored: Boolean; virtual;
    property ParamData[Index: Integer]: Variant read GetParamData write SetParamData;
  end;

  TIntegerParam = class(TCustomParam)
  private
    FParamData: DWORD;
    FAddrStore: Boolean;
    function GetParamData(Index: Integer): Variant;  override;
    procedure SetParamData(Index: Integer; const Value: Variant); override;
  public
    constructor Create(AddrStore: Boolean = False);
    procedure WriteToStream(Stream: TStream); override;
    procedure ReadFromStream(Stream: TStream); override;
    function IsAddrSored: Boolean; override;
  end;

  TFloatParam = class(TCustomParam)
    private
    FParamData: Single;
    function GetParamData(Index: Integer): Variant;  override;
    procedure SetParamData(Index: Integer; const Value: Variant); override;
  public
    procedure WriteToStream(Stream: TStream); override;
    procedure ReadFromStream(Stream: TStream); override;
  end;


  TWordParam = class(TCustomParam)
  private
    FParamData: Word;
    function GetParamData(Index: Integer): Variant;  override;
    procedure SetParamData(Index: Integer; const Value: Variant); override;
  public
    procedure WriteToStream(Stream: TStream); override;
    procedure ReadFromStream(Stream: TStream); override;
  end;

  TByteParam = class(TCustomParam)
  private
    FParamData: Byte;
    function GetParamData(Index: Integer): Variant;  override;
    procedure SetParamData(Index: Integer; const Value: Variant); override;
  public
    procedure WriteToStream(Stream: TStream); override;
    procedure ReadFromStream(Stream: TStream); override;
  end;

  TStringParam = class(TCustomParam)
  private
    FParamData: AnsiString;
    FStringStream: TMemoryStream;
    FStringList: TStringList;
    function GetParamData(Index: Integer): Variant;  override;
    procedure SetParamData(Index: Integer; const Value: Variant); override;
  public
    constructor Create(StringStream: TMemoryStream; StringList: TStringList);
    function StringStream: TMemoryStream;
    function StringList: TStringList;
    procedure WriteToStream(Stream: TStream); override;
    procedure ReadFromStream(Stream: TStream); override;
  end;


  TWordParamSet = class(TCustomParam)
  private
    FParamDataList: TList;
    function GetParamData(Index: Integer): Variant;  override;
    procedure SetParamData(Index: Integer; const Value: Variant); override;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Clear; override;
    procedure WriteToStream(Stream: TStream); override;
    procedure ReadFromStream(Stream: TStream); override;
    function Count: Integer; override;
  end;

  TByteParamSet = class(TCustomParam)
  private
    FParamDataList: TList;
    function GetParamData(Index: Integer): Variant;  override;
    procedure SetParamData(Index: Integer; const Value: Variant); override;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Clear; override;
    procedure WriteToStream(Stream: TStream); override;
    procedure ReadFromStream(Stream: TStream); override;
    function Count: Integer; override;
  end;

  TFloatParamSet = class(TCustomParam)
  private
    FParamDataList: TList;
    function GetParamData(Index: Integer): Variant;  override;
    procedure SetParamData(Index: Integer; const Value: Variant); override;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Clear; override;
    procedure WriteToStream(Stream: TStream); override;
    procedure ReadFromStream(Stream: TStream); override;
    function Count: Integer; override;
  end;

  TIntegerParamSet = class(TCustomParam)
  private
    FAddrStore: Boolean;
    FParamDataList: TList;
    function GetParamData(Index: Integer): Variant;  override;
    procedure SetParamData(Index: Integer; const Value: Variant); override;
  public
    constructor Create(AddrStore: Boolean = False);
    destructor Destroy; override;
    procedure Clear; override;
    procedure WriteToStream(Stream: TStream); override;
    procedure ReadFromStream(Stream: TStream); override;
    function Count: Integer; override;
    function IsAddrSored: Boolean; override;
  end;

  TStringParamSet = class(TCustomParam)
  private
    FParamDataList: TList;
    function GetParamData(Index: Integer): Variant;  override;
    procedure SetParamData(Index: Integer; const Value: Variant); override;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Clear; override;
    procedure WriteToStream(Stream: TStream); override;
    procedure ReadFromStream(Stream: TStream); override;
    function Count: Integer; override;
  end;



  TParamItem = class(TObject)
  protected
    FParamData: TObject;
    FSectionCount: Integer;
    FRowCount: Integer;
    FSize: Integer;
    FItemsList: TList;
    FFieldsList: TList;
    FIsDefinesParam: Boolean;
    bByte: Byte;
    bNewFld: BYTE;
    wColCount: WORD;
    wSize: WORD;
    wnSize: WORD;
    FLastIdx: Integer;
    FFileldsNames: TStringList;

  public
    FIndex: DWord;
    FIsImported: Boolean;
    FParamName: TStringParam;
  public
    constructor Create();
    destructor Destroy; override;
    procedure AssignAll(Source: TParamItem; StringStream: TMemoryStream; StringList, AConstNameList, BConstNameList, FixUpList, FObjRefList: TStringList; ClearValues: Boolean = False);
    procedure ParseToListView(ListView: TListView);
    procedure WriteToStream(Stream: TStream; Defines: TMemoryStream ; StrList: TStringList; ObjList: TStringList; AddObjList: TStringList; var AIdx: Integer);
    procedure ReadFromStream(Stream: TStream; Defines: TMemoryStream ; StrList: TStringList;
       ObjList: TStringList; List: TList; FileldsNames: TList; AddObjList: TStringList);
  end;

  TForm1 = class(TForm)
    LEObjList: TListView;
    ToolBar1: TToolBar;
    TBOpen: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    StringGrid1: TStringGrid;
    BtnDwnLst: TButton;
    PMenuShowState: TPopupMenu;
    Shownamesonly1: TMenuItem;
    Shownamesandindex1: TMenuItem;
    Shownamesandindexhex1: TMenuItem;
    ImageList1: TImageList;
    ToolButton1: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    Image1: TImage;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    Panel1: TPanel;
    Edit1: TEdit;
    StatusBar1: TStatusBar;
    CheckBox1: TCheckBox;
    PEditMenu: TPopupMenu;
    EditName1: TMenuItem;
    Showtypes1: TMenuItem;
    ShowID1: TMenuItem;
    Signedinteger1: TMenuItem;
    Unsignedinteger1: TMenuItem;
    HEX1: TMenuItem;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    PMGrid: TPopupMenu;
    Addnewitem1: TMenuItem;
    Copyitem1: TMenuItem;
    Removeitem1: TMenuItem;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    save1: TMenuItem;
    Exit1: TMenuItem;
    ools1: TMenuItem;
    Joinparfiles1: TMenuItem;
    Reloadchanges1: TMenuItem;
    Options1: TMenuItem;
    Savecolumnsnames1: TMenuItem;
    Clearallfilters1: TMenuItem;
    Edit2: TMenuItem;
    Addnewitem2: TMenuItem;
    Diplicateitem1: TMenuItem;
    removeitem2: TMenuItem;
    Memo1: TMemo;
    ProgressBar1: TProgressBar;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    Batchedit1: TMenuItem;
    Panel2: TPanel;
    SpeedButton2: TSpeedButton;
    Edit3: TEdit;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    FindALLCB: TCheckBox;
    Splitter2: TSplitter;
    procedure TBOpenClick(Sender: TObject);
    procedure LEObjListChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ReadDefines(Stream: TStream; Stream2: TStream = nil);
    procedure FillDefineStringGrid();
    procedure FillObjNumStringGrid();
    procedure WriteDefines(Stream: TStream);
    procedure SetEditTextToCell(Text: String; ACol, ARow: Integer);
    function GetEditTextFromCell(ACol, ARow: Integer; IgnoreNames: Boolean = True): String;

    procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure BtnDwnLstClick(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure StringGrid1KeyPress(Sender: TObject; var Key: Char);
    procedure StringGrid1Click(Sender: TObject);
    procedure StringGrid1GetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure Splitter1Paint(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure LEObjListResize(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure StringGrid1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ToolButton4Click(Sender: TObject);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditName1Click(Sender: TObject);
    procedure Showtypes1Click(Sender: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure ToolButton12Click(Sender: TObject);
    procedure ToolButton13Click(Sender: TObject);
    procedure Addnewitem1Click(Sender: TObject);
    procedure Joinparfiles1Click(Sender: TObject);
    procedure Batchedit1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
    FFile: TFileStream;
    FList: TList;
    FConstNameList: TStringList;
    FDefinesList: TStringList;
    FObjRefList: TStringList;
    FCurItem: TObject;
    GUID: TGUID;
    FDW1: DWORD;
    FDW2: DWORD;
    FSize: DWORD;
    FNewSize: DWORD;
    FDW3: DWORD;
    FVer: DWORD;
    FDefinesStream: TMemoryStream;
    FIsEditing: Boolean;
    FEditTexst: String;
    FOldEditText: String;
    FACol: DWORD;
    FARow: DWORD;
    FAhCol: longInt;
    FAhRow: LongInt;
    FIniSettings: TIniFile;
    FLastFound: Integer;
    FMinColSz: Integer;
    FIsMBtnDown: Boolean;
    FIsLocked: Boolean;
    FFileldsNames: TList;
    FFilter: String;
//    sinf: STARTUPINFO;
//  	pinf: PROCESS_INFORMATION;
//    procedure FillParamList(Params: TObject);
    function GetColumnType(ACol, ARow: Integer): Integer;
    procedure CheckItem(Idx: Integer);
    function UpdTextInCell(par: TParamItem; ACol, ARow: Integer; IgnoreNames: Boolean = False): String;
    function GetTypeString(Typ: Integer): String;
    procedure Clear;
    procedure FindAll(const Text: String; Strings: TStrings);
  public
    FFileNameA: String;
    FFileNameB: String;
    FFileNameC: String;
    { Public declarations }
  end;

  function GetMString(Index: DWORD; DefinesStream: TMemoryStream): String;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.Addnewitem1Click(Sender: TObject);
var
  par, par2: TParamItem;
  List: TList;
  Idx: Integer;
  CurItem: TObject;
  Index: Integer;
begin
  if FCurItem = nil then Exit;
  List := TList(FCurItem);
  if (Sender = Copyitem1) then
    Index := FARow - 1
  else
    Index := List.Count - 1;

  if (Index < 0) then Exit;

  par := List[Index];
  par2 := TParamItem.Create;
  par2.AssignAll(par, FDefinesStream, FDefinesList, FConstNameList, FConstNameList, nil, FObjRefList, Sender <> Copyitem1);
  if Index + 1 >= List.Count then
    List.Add(par2)
  else
    List.Insert(Index + 1, par2);
  index := par.FIndex + 1;
  if Index + 1 >= FConstNameList.Count then
    FConstNameList.AddObject(par2.FParamName.ParamData[0], par2)
  else
    FConstNameList.InsertObject(par.FIndex + 1, par2.FParamName.ParamData[0], par2);
  Inc(FSize);
  Inc(FNewSize);
  for Idx := 0 to FConstNameList.Count - 1 do
    TParamItem(FConstNameList.Objects[Idx]).FIndex := Idx;
  CurItem := FCurItem;
  FCurItem := nil;
  LEObjListChange(nil, LEObjList.Items[LEObjList.ItemIndex], TItemChange(0));
  FCurItem := CurItem;

end;

procedure TForm1.Batchedit1Click(Sender: TObject);
var
  List: TList;
  par: TParamItem;
  param: TCustomParam;
  FilterList: TStringList;
  i, j, idx, iStart, iEnd: Integer;
begin
  if (FAhCol < 2) or (FAhRow = -1) then Exit;
  if FCurItem = nil then Exit;
  List := TList(FCurItem);

  par := TParamItem(List[0]);
  param := TCustomParam(par.FItemsList[FAhCol - 2]);
  BatchEditForm.FIsText := param is TStringParam;

  if BatchEditForm.ShowModal = mrOk then
  begin

     FilterList := TStringList.Create;
    FilterList.Delimiter := ';';
    FilterList.DelimitedText := FFilter;
    iStart := 0;
    if FilterList.Count > 0 then
      iEnd := FilterList.Count - 1
    else
      iEnd := List.Count - 1;

    if (BatchEditForm.FIdxStart > -1) and (BatchEditForm.FIdxStart <= iEnd) then
      iStart := BatchEditForm.FIdxStart;
    if (BatchEditForm.FIdxEnd <= iEnd) and (BatchEditForm.FIdxEnd > 0) then
      iEnd := BatchEditForm.FIdxEnd;

    for i := iStart to iEnd do
    begin
      if FilterList.Count > 0 then
        Idx := StrToInt(FilterList[i]) - 1
      else
        Idx := i;
      par := TParamItem(List[Idx]);
      param := TCustomParam(par.FItemsList[FAhCol - 2]);
      for j := 0 to param.Count - 1 do
      begin
        case BatchEditForm.FOp of
          1: param.ParamData[j] := param.ParamData[j] + BatchEditForm.FValue;
          2: param.ParamData[j] := param.ParamData[j] - BatchEditForm.FValue;
          3: param.ParamData[j] := param.ParamData[j] * BatchEditForm.FValue;
          4: param.ParamData[j] := param.ParamData[j] / BatchEditForm.FValue;
          else
            param.ParamData[j] := BatchEditForm.FValue;
        end;
      end;
      StringGrid1.Cells[FAhCol, Idx + 1] := UpdTextInCell(par, FAhCol - 2, Idx - 1);
    end;
    FilterList.Free;
  end;
end;

procedure TForm1.BtnDwnLstClick(Sender: TObject);
begin
  PMenuShowState.Popup(Self.Left + BtnDwnLst.Left, StringGrid1.Top + Self.Top + BtnDwnLst.Top);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
  begin
    Panel1.Width := 336;
    Edit1.Enabled := True;
  end
  else
  begin
    Panel1.Width := 96;
    Edit1.Enabled := False;
  end;
end;

procedure TForm1.CheckItem(Idx: Integer);
var
  i: Integer;
begin
  for I := 1 to PEditMenu.Items.Count - 1 do
      PEditMenu.Items[i].Checked := (I = Idx)
end;

procedure TForm1.Clear;
var
  idx, Idx2: Integer;
  FObjList: Tlist;
begin
  for Idx := 0 to LEObjList.Items.Count - 1 do
  begin
    if (LEObjList.Items[Idx].Data <> nil) and (TObject(LEObjList.Items[Idx].Data) is TList) then
    begin
      FObjList := TList(LEObjList.Items[Idx].Data);
      for idx2 := 0 to FObjList.Count - 1 do
        if TObject(FObjList[idx2]) is TParamItem then
        begin
          TParamItem(FObjList[idx2]).Free;
          FObjList[idx2] := nil;
        end;
      FObjList.Free;
    end;
    LEObjList.Items[Idx].Data := nil;
  end;
  LEObjList.Items.BeginUpdate;
  LEObjList.Items.Clear;
  LEObjList.Items.EndUpdate;
  for Idx := 0 to Flist.Count - 1 do
    if TObject(Flist[Idx]) is TObject then
      TObject(Flist[Idx]).Free;
  for Idx := 0 to FFileldsNames.Count - 1 do
    if TObject(FFileldsNames[Idx]) is TObject then
      TObject(FFileldsNames[Idx]).Free;
  for Idx := 0 to FObjRefList.Count - 1 do
    if TObject(FObjRefList.Objects[Idx]) is TObject then
      TObject(FObjRefList.Objects[Idx]).Free;
  Flist.Clear;
  FDefinesStream.SetSize(0);
  FDefinesStream.Position := 0;
  FConstNameList.Clear;
  FFileldsNames.Clear;
  FObjRefList.Clear;
end;

procedure TForm1.EditName1Click(Sender: TObject);
var
  s: String;
begin
  if (FAhCol < 2) or (FAhRow = -1) then Exit;
  if (LEObjList.ItemIndex >= FFileldsNames.Count) or (LEObjList.ItemIndex < 0) then Exit;
  if not(TObject(FFileldsNames[LEObjList.ItemIndex]) is TStringList) then  Exit;
  s := TStringList(FFileldsNames[LEObjList.ItemIndex]).Strings[FAhCol - 2];

  s := InputBox('Edit column caption', 'enter a new column caption here:', s);
  if s <> '' then
  begin
    TStringList(FFileldsNames[LEObjList.ItemIndex]).Strings[FAhCol - 2] := s;
///    LEObjListChange(Self, LEObjList.Items[LEObjList.ItemIndex], TItemChange(nil));
  end;
end;

procedure TForm1.FillDefineStringGrid;
var
  BuffPtr: PByte;
  Idx, ItemIdx, PrevIdx: Integer;
  Str: AnsiString;
begin
    { parse defines }
    BuffPtr := FDefinesStream.Memory;
    Idx := 0;
    ItemIdx := 0;
    PrevIdx := 0;

    StringGrid1.ColCount := 2;
    while (Idx < FDefinesStream.Size) do
    begin
      if BuffPtr[Idx] = 0 then
      begin
       if PrevIdx <> 0 then
       begin
        SetLength(Str, Idx - PrevIdx);
        FDefinesStream.Position := PrevIdx;
        FDefinesStream.Read(Str[1], Idx - PrevIdx);
//        Item.SubItems.Add(Str);
        StringGrid1.RowCount := ItemIdx + 1;
        StringGrid1.Cells[0, ItemIdx] := IntToStr(PrevIdx);
        StringGrid1.Cells[1, ItemIdx] := String(Str);
       end;
        Inc(ItemIdx);

        PrevIdx := Idx + 1;
      end;
      Inc(Idx);
    end;
end;

procedure TForm1.FillObjNumStringGrid;
var
  Idx: Integer;
begin
    StringGrid1.ColCount := 2;
    StringGrid1.RowCount := FConstNameList.Count + 1;
    for Idx := 0 to FConstNameList.Count - 1 do
    begin
      StringGrid1.Cells[0, Idx + 1] := IntToStr(Idx);
      StringGrid1.Cells[1, Idx + 1] := FConstNameList.Strings[Idx];
    end;
end;

procedure TForm1.FindAll(const Text: String; Strings: TStrings);
var
  LObjList: TList;
  i, j, k: Integer;
  par: TParamItem;
  s, s1: String;
begin
  if CheckBox2.Checked then
    s1 := Text
  else
    s1 := AnsiUpperCase(Text);
  for i := 0 to LEObjList.Items.Count - 1 do
  begin
    LObjList := TList(LEObjList.Items[i].Data);
    if Assigned(LObjList) then
    begin
      for j := 0 to LObjList.Count - 1 do
      begin
        par := TParamItem(LObjList[j]);
        for k := 0 to par.FItemsList.Count - 1 do
        begin
          s := UpdTextInCell(par, k, j);
          if not CheckBox2.Checked then
            s := AnsiUpperCase(s);
          if Pos(s1, s) > 0 then
            Strings.Add(Format('Found at [%u,%u] Item name: %s. Value: %s', [j, k, VarToStr(par.FparamName.ParamData[0]), s]));
        end;
      end;
    end;
  end;
end;

{
procedure TForm1.FillParamList(Params: TObject);
begin
    if Params = nil then Exit;
    LEParams.ViewStyle := vsReport;
    if Params is TStringList then
      with TStringList(Params) do
      begin
        with LEParams.Columns.Add do
        begin
          Index := 0;
          Caption := 'Idx';
        end;
        with LEParams.Columns.Add do
        begin
          Index := 1;
          Caption := 'Define string';
          Width := 200;
        end;
        for Idx:= 0 to Count - 1 do
        with LEParams.Items.Add do
        begin
          Caption := IntTostr(Integer(Objects[Idx]));
          SubItems.Add(Strings[Idx]);
        end;
      end;

end;
}

procedure TForm1.FormCreate(Sender: TObject);
var
  s: String;
begin
  FList := TList.Create;
  FIsLocked := False;
  FVer := $00015250;
  FDefinesStream := TMemoryStream.Create;
  FConstNameList := TStringList.Create;
  FFileldsNames := TList.Create;
  FDefinesList := TStringList.Create;
  FDefinesList.Sorted := True;
  FDefinesList.CaseSensitive := True;
  FObjRefList := TStringList.Create;
  FObjRefList.Sorted := False;
  FObjRefList.CaseSensitive := False;
  FObjRefList.Duplicates := dupIgnore;
  ToolButton2.Enabled := False;
  ToolButton3.Enabled := False;
  ToolButton10.Enabled := False;
  ToolButton13.Enabled := False;
  FFileNameA := '';
  FFileNameB := '';
  FFileNameC := '';

  FIniSettings := TIniFile.Create(Copy(Application.ExeName, 1, Length(Application.ExeName) - 3) + 'ini');
  s := FIniSettings.ReadString('Options', 'MixColWidth', '30');
  if not TryStrToInt(s, FMinColSz) then
    FMinColSz := 30;
  if FMinColSz > 300 then
    FMinColSz := 300;
  if FMinColSz < 0 then
    FMinColSz := 0;
  //Label1.Caption := 'Two Worlds  2 parameters editor' + #10#13 + 'by HeliX666';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Clear;
  FDefinesList.Free;
  FDefinesStream.Free;
  FConstNameList.Free;
  FIniSettings.Free;
  Flist.Free;
  FFileldsNames.Free;
  FObjRefList.Free;
end;

function TForm1.GetColumnType(ACol, ARow: Integer): Integer;
var
  Lst: TList;
begin
  Result := -1;
  if not(FCurItem is TList)  then Exit;
  Lst := TList(FCurItem);
  Result := Byte(TParamItem(Lst[0]).FFieldsList[ACol - 1]);
end;

function TForm1.GetEditTextFromCell(ACol, ARow: Integer; IgnoreNames: Boolean = True): String;
var
  lst: TList;
  par: TParamItem;
  FilterList: TStringList;
begin
  Result := '';

  if FCurItem is TList then
  begin
     lst := TList(FCurItem);
    par := TParamItem(lst[ARow - 1]);
    {typ := Integer(TParamItem(lst[ARow - 1]).FFieldsList[ACol - 1]);
    for Idx := 0 to par.Count - 1 do
      if (par is TFloatParamSet) and (Idx > 0)  then
        Str := Str + ';' + Format('%f', [Single(par.ParamData[Idx])])
      else if par is TFloatParam then
        Str := Format('%f', [Single(par.ParamData[Idx])])
      else if (par is TStringParamSet) and (Idx > 0)  then
        Str := Str + ';' + String(par.ParamData[Idx])
      else if ((par is TIntegerParamSet) or (par is TWordParamSet) or (par is TByteParamSet)) and (Idx > 0)  then
        Str := Str + ';' + VarToStr(par.ParamData[Idx])
      else
        Str := VarToStr(par.ParamData[Idx]);
        }
    FilterList := TStringList.Create;
    FilterList.Delimiter := ';';
    FilterList.DelimitedText := FFilter;
    if FilterList.Count > 0 then
    begin
      ARow := StrToInt(FilterList[ARow - 1]);
      par := TParamItem(lst[ARow - 1]);
      if (ACol = 0) then
        Result := par.FParamName.ParamData[0]
      else
        Result := UpdTextInCell(par, ACol - 1, ARow - 1);
     FilterList.Free;
    Exit;
    end;
    FilterList.Free;
    if (ACol = 0) then
      Result := par.FParamName.ParamData[0]
    else
      Result := UpdTextInCell(par, ACol - 1, ARow - 1);
  end;
end;

function GetMString(Index: DWORD; DefinesStream: TMemoryStream): String;
   var
    AStr: AnsiString;
    strIdx: DWORD;
    BuffPtr: PByte;
    Ln: Integer;
   begin
        BuffPtr := DefinesStream.Memory;
        AStr := '';
        Ln := 255;
//        Cn := 0;
        strIdx := Index;
        SetLength(AStr, Ln);
        while BuffPtr[StrIdx] <> 0 do
        begin
          Inc(StrIdx);
        end;
        SetLength(AStr, StrIdx - Index);
        CopyMemory(@AStr[1], @BuffPtr[Index], StrIdx - Index);
        Result := String(AStr);
   end;

function TForm1.GetTypeString(Typ: Integer): String;
begin
  case Typ of
  0: Result := 'Byte';
  1: Result := 'Word';
  2: Result := 'Integer';
  3: Result := 'Float';
  4: Result := 'ID';
  5: Result := 'String';
  7: Result := 'Byte set';
  8: Result := 'Word set';
  9: Result := 'Int. set';
  10: Result := 'Float set';
  11: Result := 'ID set';
  12: Result := 'String set';
  else
    Result := 'Unknown';
  end;
end;

procedure TForm1.Joinparfiles1Click(Sender: TObject);
var
  ADefinesStream, BDefinesStream, Mem: TMemoryStream;
  ADefinesList, AConstNameList, BDefinesList, BConstNameList: TStringList;
  AObjRefList: TStringList;
  BObjRefList, FixUpList: TStringList;
  DWRead: DWORD;
  Item, Idx, ItemIdx, PrevIdx, i , j, LastParIdx, ItemIdx2: Integer;
  ParamItem, ParamItem2 , NewPar: TParamItem;
  FObjList, NewList, List: TList;
  AFile, BFile: TFileStream;
  AGUID: TGUID;
  ASize, BSize: DWORD;
  AList, AFNamesList, BList, BFNamesList, lst: TList;
  par, par2: TCustomParam;
  IsCompared, fFound: Boolean;
begin
  IsCompared := Sender = ToolButton13;
  if (IsCompared) and (LEObjList.Items.Count = 0) then
  begin
    MessageDlg('You shold load a file first. Use the open button.', mtError, [mbOK], 0);
    Exit;
  end;
  if IsCompared then
    StatusBar1.Panels.Items[1].Text := 'Joining...'
  else
    StatusBar1.Panels.Items[1].Text := 'Comparing...';
  ADefinesStream := TMemoryStream.Create;
  BDefinesStream := TMemoryStream.Create;
  ADefinesList := TStringList.Create;
  AConstNameList := TStringList.Create;
  AObjRefList := TStringList.Create;
  BDefinesList := TStringList.Create;
  BConstNameList  := TStringList.Create;
  BObjRefList := TStringList.Create;
  AList := TList.Create;
  AFNamesList := TList.Create;
  BList := TList.Create;
  BFNamesList := TList.Create;
  NewList := TList.Create;
  FixUpList := TStringList.Create;
  List := nil;
  try
    OpenDialog1.Title := 'Open original file';

    if FFileNameB = '' then
      if not OpenDialog1.Execute() then Exit;

    begin
      if FFileNameB <> '' then
        AFile := TFileStream.Create(FFileNameB, 0)
      else
        AFile := TFileStream.Create(OpenDialog1.FileName, 0);
      AFile.Read(DWRead, 4);
      if (DWRead <> $00015250) and (DWRead <> $0005250) then
      begin
        //LBErrorLog.AddItem('Couldn''''t open: Incorrect file type', nil);
        MessageDlg('Couldn''''t open: Incorrect file type', mtError, [mbOK], 0);
        AFile.Free;
        Exit;
      end;
      FVer := DWRead;
      FIsLocked := True;
      Memo1.Clear;
      StringGrid1.Enabled := False;
      LEObjList.Enabled := False;
      ToolBar1.Enabled := False;
      AFile.Read(DWRead, 4);
      AFile.Read(DWRead, 4);
      AFile.Read(AGUID, 16);
      AFile.Read(DWRead, 4);
      ADefinesStream.SetSize(DWRead);
      ADefinesStream.CopyFrom(AFile, DWRead);

      AFile.Read(ASize, 4);
      AFile.Read(ASize, 4);
      AFile.Read(DWRead, 4);

      for Idx := 0 to ASize - 1 do
      begin
        ParamItem := TParamItem.Create;
        AConstNameList.AddObject('', ParamItem);
      end;
      ProgressBar1.Min := 1;
      ProgressBar1.Max := ASize * 10;
      ProgressBar1.Step := 1;
      for Idx := 0 to ASize - 1 do
      begin
        ProgressBar1.StepIt;
        Application.ProcessMessages;
        ParamItem := TParamItem(AConstNameList.Objects[Idx]);
        PrevIdx := Flist.Count;
        ParamItem.ReadFromStream(AFile, ADefinesStream, ADefinesList, AConstNameList, AList, AFNamesList, AObjRefList);
        ParamItem.FIndex := Idx;
        AConstNameList.Strings[Idx] := ParamItem.FparamName.ParamData[0];
      end;

       if AFile.Position < AFile.Size  then
       begin
         AFile.Read(DWRead, 4);
         AFile.Read(DWRead, 4);
       end;
       Idx := 0;
       while AFile.Position < AFile.Size do
       begin
         if Idx < AObjRefList.Count -1  then
           TParamItem(AObjRefList.Objects[Idx]).FParamName.ReadFromStream(AFile)
         else
         begin
           AObjRefList.AddObject(IntToStr(Idx), TParamItem.Create);
           TParamItem(AObjRefList.Objects[Idx]).FIsImported := True;
           TParamItem(AObjRefList.Objects[Idx]).FParamName.ReadFromStream(AFile);
         end;
         AObjRefList.Strings[Idx] := TParamItem(AObjRefList.Objects[Idx]).FParamName.ParamData[0];
         TParamItem(AObjRefList.Objects[Idx]).FIndex := Idx;
         Inc(Idx);
       end;
      AFile.Free;
      OpenDialog1.Title := 'Open changed file';

      if FFileNameC = '' then
        if not OpenDialog1.Execute() then Exit;
      begin
        if FFileNameC <> '' then
          BFile := TFileStream.Create(FFileNameC, 0)
        else
          BFile := TFileStream.Create(OpenDialog1.FileName, 0);
        //BFile := TFileStream.Create(OpenDialog1.FileName, 0);
        BFile.Read(DWRead, 4);
        if (DWRead <> $00015250) and (DWRead <> $0005250) then
        begin
          MessageDlg('Couldn''''t open: Incorrect file type', mtError, [mbOK], 0);
          Exit;
        end;
        FVer := DWRead;
        BFile.Read(DWRead, 4);
        BFile.Read(DWRead, 4);
        BFile.Read(AGUID, 16);
        BFile.Read(DWRead, 4);
        BDefinesStream.SetSize(DWRead);
        BDefinesStream.CopyFrom(BFile, DWRead);

        BFile.Read(BSize, 4);
        BFile.Read(BSize, 4);
        BFile.Read(DWRead, 4);

      for Idx := 0 to BSize - 1 do
      begin
        ParamItem := TParamItem.Create;
        BConstNameList.AddObject('', ParamItem);
      end;
      for Idx := 0 to BSize - 1 do
      begin
        ProgressBar1.StepIt;
        Application.ProcessMessages;
        ParamItem := TParamItem(BConstNameList.Objects[Idx]);
        //PrevIdx := Flist.Count;
        ParamItem.ReadFromStream(BFile, BDefinesStream, BDefinesList, BConstNameList, BList, BFNamesList, BObjRefList);
        ParamItem.FIndex := Idx;
        BConstNameList.Strings[Idx] := ParamItem.FparamName.ParamData[0];
      end;

      if BFile.Position < BFile.Size  then
      begin
        BFile.Read(DWRead, 4);
        BFile.Read(DWRead, 4);
      end;
      Idx := 0;
      while BFile.Position < BFile.Size do
      begin
        if Idx < BObjRefList.Count -1  then
          TParamItem(BObjRefList.Objects[Idx]).FParamName.ReadFromStream(BFile)
        else
        begin
          BObjRefList.AddObject(IntToStr(Idx), TParamItem.Create);
          TParamItem(BObjRefList.Objects[Idx]).FIsImported := True;
          TParamItem(BObjRefList.Objects[Idx]).FParamName.ReadFromStream(BFile);
        end;
        BObjRefList.Strings[Idx] := TParamItem(BObjRefList.Objects[Idx]).FParamName.ParamData[0];
        TParamItem(BObjRefList.Objects[Idx]).FIndex := Idx;
        Inc(Idx);
      end;

      PrevIdx := -1;
      for Idx := 0 to BSize - 1 do
      begin
        ParamItem2 := TParamItem(BConstNameList.Objects[Idx]);
        ProgressBar1.StepBy(8);
        Application.ProcessMessages;
        Item := AConstNameList.IndexOf(ParamItem2.FparamName.ParamData[0]);
        if Item <> -1 then
        begin
          ParamItem := TParamItem(AConstNameList.Objects[Item]);
          for i := 0 to ParamItem.FItemsList.Count - 1 do
          begin
            par2 := TCustomParam(ParamItem2.FItemsList[i]);
            par := TCustomParam(ParamItem.FItemsList[i]);
            if (par.Count <> par2.Count) and IsCompared then
            begin
              PrevIdx := FConstNameList.IndexOf(ParamItem2.FparamName.ParamData[0]);
              if PrevIdx <> - 1 then
              begin
                NewPar := TParamItem(FConstNameList.Objects[PrevIdx]);
                TCustomParam(NewPar.FItemsList[i]).Clear;
              end;
            end;
            LastParIdx := 0;
            for j := 0 to par2.Count - 1 do
              if (par2.IsAddrSored) and (par.IsAddrSored) then
              begin
                 if par.Count <> par2.Count then
                 begin
                   if IsCompared then
                   begin
                     PrevIdx := FConstNameList.IndexOf(ParamItem2.FparamName.ParamData[0]);
                     if PrevIdx <> - 1 then
                     begin
                       NewPar := TParamItem(FConstNameList.Objects[PrevIdx]);
                       PrevIdx := FConstNameList.IndexOf(TParamItem(DWORD(par2.ParamData[j])).FParamName.ParamData[0]);
                       if PrevIdx <> -1 then
                         TCustomParam(NewPar.FItemsList[i]).ParamData[-1] := DWORD(FConstNameList.Objects[PrevIdx])
                       else
                       begin
                         PrevIdx := FObjRefList.IndexOf(TParamItem(DWORD(par2.ParamData[j])).FParamName.ParamData[0]);
                         if PrevIdx <> -1 then
                           TCustomParam(NewPar.FItemsList[i]).ParamData[-1] := DWORD(FObjRefList.Objects[PrevIdx])
                         else
                         begin
                           if TParamItem(DWORD(par2.ParamData[j])).FIsImported then
                           begin
                             NewPar := TParamItem.Create;
                             NewPar.FParamName.ParamData[0] := TParamItem(DWORD(par2.ParamData[j])).FParamName.ParamData[0];
                             NewPar.FIndex := FObjRefList.Count;
                             NewPar.FIsImported := True;
                             FObjRefList.AddObject(NewPar.FParamName.ParamData[0], NewPar);
                           end;

                           PrevIdx :=  FixUpList.IndexOf(ParamItem2.FparamName.ParamData[0]);
                           if PrevIdx = -1 then
                           begin
                             FixUpList.AddObject(ParamItem2.FparamName.ParamData[0], TList.Create);
                             PrevIdx := FixUpList.Count - 1;
                           end;
                           if LastParIdx = 0 then
                           begin
                            TList(FixUpList.Objects[PrevIdx]).Add(TObject(i));
                             inc(LastParIdx);
                           end;

                         end;

                       end;
                     end;
                   end;
                   Memo1.Lines.Add('Item #' + ParamItem.FparamName.ParamData[0]
                    + ' in column ' + intToStr(i + 1) + ' has dif. set value');
                 end
                else if TParamItem(DWORD(par2.ParamData[j])).FParamName.ParamData[0] <> TParamItem(DWORD(par.ParamData[j])).FParamName.ParamData[0] then
                begin
                  Memo1.Lines.Add('Item #' + ParamItem.FparamName.ParamData[0]
                  + ' in column ' + intToStr(i + 1) + ' has dif. index value: old "'
                  + TParamItem(DWORD(par.ParamData[j])).FParamName.ParamData[0] + '" new "'
                  + TParamItem(DWORD(par2.ParamData[j])).FParamName.ParamData[0]  + '"');
                  //if PrevIdx <> Idx then
                  //begin
                   // NewList.Add(TStringList.Create);
                  //  PrevIdx := Idx
                  //end;
                  if IsCompared then
                  begin
                    PrevIdx := FConstNameList.IndexOf(ParamItem2.FparamName.ParamData[0]);
                    if PrevIdx <> - 1 then
                    begin
                      NewPar := TParamItem(FConstNameList.Objects[PrevIdx]);
                      PrevIdx := FConstNameList.IndexOf(TParamItem(DWORD(par2.ParamData[j])).FParamName.ParamData[0]);
                      if PrevIdx <> -1 then
                        TCustomParam(NewPar.FItemsList[i]).ParamData[j] := DWORD(FConstNameList.Objects[PrevIdx])
                      else
                    begin
                      PrevIdx := FObjRefList.IndexOf(TParamItem(DWORD(par2.ParamData[j])).FParamName.ParamData[0]);
                      if PrevIdx <> -1 then
                        TCustomParam(NewPar.FItemsList[i]).ParamData[j] := DWORD(FObjRefList.Objects[PrevIdx])
                      else
                      begin
                        if TParamItem(DWORD(par2.ParamData[j])).FIsImported then
                        begin
                          NewPar := TParamItem.Create;
                          NewPar.FParamName.ParamData[0] := TParamItem(DWORD(par2.ParamData[j])).FParamName.ParamData[0];
                          NewPar.FIndex := FObjRefList.Count;
                          NewPar.FIsImported := True;
                          FObjRefList.AddObject(NewPar.FParamName.ParamData[0], NewPar);
                        end;
                        PrevIdx :=  FixUpList.IndexOf(ParamItem2.FparamName.ParamData[0]);
                        if PrevIdx = -1 then
                        begin
                          FixUpList.AddObject(ParamItem2.FparamName.ParamData[0], TList.Create);
                          PrevIdx := FixUpList.Count - 1;
                        end;
                        if LastParIdx = 0 then
                        begin
                          TList(FixUpList.Objects[PrevIdx]).Add(TObject(i));
                          inc(LastParIdx);
                        end;
                      end;
                    end;
                    end
                  end;
                end;
              end
              else if par.Count <> par2.Count then
                 begin
                   if IsCompared then
                   begin
                     PrevIdx := FConstNameList.IndexOf(ParamItem2.FparamName.ParamData[0]);
                     if PrevIdx <> - 1 then
                     begin
                       NewPar := TParamItem(FConstNameList.Objects[PrevIdx]);
                       TCustomParam(NewPar.FItemsList[i]).ParamData[-1] := par2.ParamData[j];
                     end;
                   end;
                   Memo1.Lines.Add('Item #' + ParamItem.FparamName.ParamData[0]
                    + ' in column ' + intToStr(i + 1) + ' has dif. set value');
                 end
              else if par2.ParamData[j] <> par.ParamData[j] then
              begin
                Memo1.Lines.Add('Item #' + ParamItem.FparamName.ParamData[0] + ' in column ' + intToStr(i + 1) + ' has dif. values: old "'
                  + VarToStr(par.ParamData[j]) + '" new "'
                  + VarToStr(par2.ParamData[j]) + '"');
                  if IsCompared then
                  begin
                   PrevIdx := FConstNameList.IndexOf(ParamItem2.FparamName.ParamData[0]);
                   if PrevIdx <> - 1 then
                   begin
                     NewPar := TParamItem(FConstNameList.Objects[PrevIdx]);
                     TCustomParam(NewPar.FItemsList[i]).ParamData[j] := par2.ParamData[j];
                   end;
                  end;
                //if PrevIdx <> Idx then
               // begin
               //   NewList.Add(TStringList.Create);
               //   PrevIdx := Idx
               // end;

              end;
          end;
        end
        else
        begin
          if IsCompared then
          begin
            {if BConstNameList.Count >= FConstNameList.Count then
            begin
              fFound := False;
              PrevIdx := 0;
              for ItemIdx := 0 to LEObjList.Items.Count - 1 do
              begin
                List := TList(LEObjList.Items[ItemIdx].Data);
                if ParamItem2.FIndex - List.Count - PrevIdx <= 0  then
                begin
                  //PrevIdx := ParamItem2.FIndex - PrevIdx;
                  PrevIdx := List.Count;
                  ParamItem2.FIndex := FConstNameList.IndexOfObject(TParamItem(List[0])) + List.Count;
                  fFound := True;
                  break;
                end;
                PrevIdx := PrevIdx + List.Count;
              end;
            end
            else}
            begin
              PrevIdx := 0;
              fFound := False;
              for ItemIdx := 0 to LEObjList.Items.Count - 1 do
              begin
                List := TList(LEObjList.Items[ItemIdx].Data);
                if TParamItem(List[0]).FFieldsList.Count = ParamItem2.FFieldsList.Count then
                begin
                  fFound := True;
                  PrevIdx := ItemIdx;
                  for ItemIdx2 := 0 to TParamItem(List[0]).FFieldsList.Count - 1 do
                    if TParamItem(List[0]).FFieldsList[ItemIdx2] <> ParamItem2.FFieldsList[ItemIdx2]  then
                    begin
                      fFound := False;
                      break;
                    end;
                end;
                if fFound then
                begin
                  PrevIdx := List.Count;
                  ParamItem2.FIndex := FConstNameList.IndexOfObject(TParamItem(List[0])) + List.Count;
                  break;
                end;
              end;
            end;
            NewPar := TParamItem.Create;
            if not fFound then
              ParamItem2.FIndex := FConstNameList.Count;
            NewPar.AssignAll(ParamItem2, FDefinesStream, FDefinesList, FConstNameList, BConstNameList, FixUpList, FObjRefList);
            if not fFound then
            begin
              List := TList.Create;
              with LEObjList.Items.Add do
              begin
                Data := List;
                Text := NewPar.FParamName.ParamData[0];
                Caption := Text;// NewPar.FParamName.ParamData[0];
              end;
              NewPar.FFieldsList := TList.Create;
              NewPar.FFileldsNames := TStringList.Create;
              for ItemIdx2 := 0 to ParamItem2.FFieldsList.Count - 1 do
                NewPar.FFieldsList.Add(ParamItem2.FFieldsList[ItemIdx2]);
              for ItemIdx2 := 0 to ParamItem2.FFileldsNames.Count - 1 do
                NewPar.FFileldsNames.AddObject(ParamItem2.FFileldsNames.Strings[ItemIdx2], ParamItem2.FFileldsNames.Objects[ItemIdx2]);
              NewPar.bNewFld := 0;
            end
            else
            begin
              NewPar.FFieldsList := TParamItem(List[0]).FFieldsList;
              NewPar.FFileldsNames := TParamItem(List[0]).FFileldsNames;
            end;
            if PrevIdx >= List.Count then
              List.Add(NewPar)
            else
              List.Insert(PrevIdx, NewPar);
            if (ParamItem2.FIndex >= Cardinal(FConstNameList.Count)) or (ParamItem2.FIndex = Cardinal(-1)) then
              FConstNameList.AddObject(NewPar.FParamName.ParamData[0], NewPar)
            else
              FConstNameList.InsertObject(ParamItem2.FIndex, NewPar.FParamName.ParamData[0], NewPar);
            Inc(FSize);
            Inc(FNewSize);
          end;
          Memo1.Lines.Add('Item #' + ParamItem2.FparamName.ParamData[0] + ' not found');
        end;
      end;
      BFile.Free;
    end;

  end;
  finally
    for Idx := 0 to FConstNameList.Count - 1 do
      TParamItem(FConstNameList.Objects[Idx]).FIndex := Idx;
    for Idx := 0 to FixUpList.Count - 1 do
    begin
      PrevIdx := FConstNameList.IndexOf(FixUpList.Strings[idx]);
      if PrevIdx <> -1 then
      begin
        NewPar := TParamItem(FConstNameList.Objects[PrevIdx]);
        lst := TList(FixUpList.Objects[idx]);
        for i := 0 to lst.Count - 1 do
        begin
          par := NewPar.FItemsList[Integer(lst[i])];
          PrevIdx := BConstNameList.IndexOf(FixUpList.Strings[idx]);
          par2 := TParamItem(BConstNameList.Objects[PrevIdx]).FItemsList[Integer(lst[i])];
          par.Clear;
          for j := 0 to par2.Count - 1 do
          begin
            PrevIdx := FConstNameList.IndexOf(TParamItem(DWORD(par2.ParamData[j])).FParamName.ParamData[0]);
            if PrevIdx <> -1 then
              par.ParamData[-1] := DWORD(FConstNameList.Objects[PrevIdx])
            else
            begin
              PrevIdx := FObjRefList.IndexOf(TParamItem(DWORD(par2.ParamData[j])).FParamName.ParamData[0]);
              if PrevIdx <> -1 then
                par.ParamData[-1] := DWORD(FObjRefList.Objects[PrevIdx]);
            end;
          end;
        end;
      end;
    end;

    for Idx := 0 to Alist.Count - 1 do
      if TObject(Alist[Idx]) is TObject then
        TObject(Alist[Idx]).Free;
    for Idx := 0 to AFNamesList.Count - 1 do
      if TObject(AFNamesList[Idx]) is TObject then
        TObject(AFNamesList[Idx]).Free;
    for Idx := 0 to Blist.Count - 1 do
      if TObject(Blist[Idx]) is TObject then
        TObject(Blist[Idx]).Free;
    for Idx := 0 to BFNamesList.Count - 1 do
      if TObject(BFNamesList[Idx]) is TObject then
        TObject(BFNamesList[Idx]).Free;
    for Idx := 0 to AConstNameList.Count - 1 do
      if TObject(AConstNameList.Objects[Idx]) is TParamItem then
        TParamItem(AConstNameList.Objects[Idx]).Free;
    for Idx := 0 to BConstNameList.Count - 1 do
      if TObject(BConstNameList.Objects[Idx]) is TParamItem then
        TParamItem(BConstNameList.Objects[Idx]).Free;
    for Idx := 0 to AObjRefList.Count - 1 do
      if TObject(AObjRefList.Objects[Idx]) is TParamItem then
        TParamItem(AObjRefList.Objects[Idx]).Free;
    for Idx := 0 to BObjRefList.Count - 1 do
      if TObject(BObjRefList.Objects[Idx]) is TParamItem then
        TParamItem(BObjRefList.Objects[Idx]).Free;
    StatusBar1.Panels.Items[1].Text := '';
    StringGrid1.Enabled := True;
    LEObjList.Enabled := True;
    ToolBar1.Enabled := True;
    ProgressBar1.Position := 0;
    ADefinesStream.Free;
    BDefinesStream.Free;
    ADefinesList.Free;
    BDefinesList.Free;
    AConstNameList.Free;
    BConstNameList.Free;
    BObjRefList.Free;
    AObjRefList.Free;
    AList.Free;
    AFNamesList.Free;
    BList.Free;
    BFNamesList.Free;
    NewList.Free;
    if FFileNameC <> '' then
      Memo1.Lines.SaveToFile(ExtractFilePath(Application.ExeName) + 'LOG.TXT');
    FIsLocked := False;
    if IsCompared and (FFileNameC = '') then
      LEObjListChange(LEObjList, LEObjList.Items[LEObjList.ItemIndex], TItemChange(0));
  end;

end;

function TForm1.UpdTextInCell(par: TParamItem; ACol, ARow: Integer; IgnoreNames: Boolean = False): String;
var
   ShowTyp, Idx3: Integer;
   Str: String;
   Sn: Single;
   ColTyp: Integer;
   parItem: TCustomParam;
   DW: DWORD;
   function GetConstName(Index: DWORD):String;
   begin
     if index = 0 then
      Result := 'nil'
     else
      Result := TparamItem(Index).FParamName.ParamData[0];// FConstNameList[FConstNameList.IndexOfObject(TObject(Index))];
//     FConstNameList[];
   end;
begin
    parItem := TCustomParam(par.FItemsList[ACol]);
    ColTyp := Integer(par.FFieldsList[ACol]);
    ShowTyp := Integer(par.FFileldsNames.Objects[ACol]);
    Str := '';
    for Idx3 := 0 to parItem.Count - 1 do
    begin
      if ColTyp in [0, 1 , 2, 7, 8, 9] then
      begin
        DW :=  VarAsType(parItem.ParamData[idx3], varLongWord);
        if ShowTyp = 4 then
          Str := Str + Format('%u', [DW])
        else if ShowTyp = 5 then
          Str := Str + Format('%x', [DW])
        else
        begin
          if ColTyp in [1, 8] then
            Str := Str + VarToStr(SmallInt(parItem.ParamData[idx3]))
          else
            Str := Str + VarToStr(Integer(DW));
        end;
      end
      else if ColTyp in [3, 10] then
      begin
        Sn := Single(parItem.ParamData[idx3]);
        if ShowTyp = 4 then
          Str := Str + Format('%u', [(PDWORD(@Sn))^])
        else if ShowTyp = 5 then
          Str := Str + Format('%x', [(PDWORD(@Sn))^])
        else
          Str := Str + Format('%f', [Sn]);
      end
      else if ColTyp in [4, 11] then
      begin
        if (ShowTyp = 2) or IgnoreNames then
        begin
          if TParamItem(DWORD(parItem.ParamData[idx3])).FIsImported then
            Str := Str + Format('%u', [TParamItem(DWORD(parItem.ParamData[idx3])).FIndex + Cardinal(FConstNameList.Count)])
          else
            Str := Str + Format('%u', [TParamItem(DWORD(parItem.ParamData[idx3])).FIndex])
        end
        else
          Str := Str + GetConstName(DWORD(parItem.ParamData[idx3]));
         // Str := Str + IntToStr(parItem.ParamData[idx3]);
      end
      else if ColTyp = 5 then
      begin
        //if (ShowTyp = 2) or IgnoreNames then
        //  Str := Str + Format('%u', [DWORD(parItem.ParamData[idx3])])
        //else
        //  Str := Str + GetMString(DWORD(parItem.ParamData[idx3]));
        Str := Str + parItem.ParamData[idx3];
      end
      else if ColTyp = 12 then
      begin
          Str := Str + parItem.ParamData[idx3];
      end
      else
        Result := 'null';

      if (parItem.Count - 1) > Idx3 then
        Str := Str + ';';
    end;
  Result := Str;
end;

procedure TForm1.LEObjListChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
   FObjList: TList;
   Idx, Idx2: Integer;
   par: TParamItem;
   Str: String;
   Ln, valInt: Integer;
   FilterList: TStringList;
begin
  if FIsLocked then Exit;
  if (Item.Data <> nil) and (TObject(Item.Data) is TList) and (TObject(Item.Data) <> FCurItem) then
  begin
    FObjList := TList(Item.Data);
    FCurItem := FObjList;
    StringGrid1.ColCount := TParamItem(FObjList[0]).FFieldsList.Count + 2;
    StringGrid1.Cells[0, 0] := 'Name(ID)';
    StringGrid1.Cells[1, 0] := 'Name';
    for Idx := 1 to TParamItem(FObjList[0]).FFieldsList.Count do
    begin
      if TObject(TParamItem(FObjList[0]).FFileldsNames) is TStringList then
      begin
        Str := TStringList(TParamItem(FObjList[0]).FFileldsNames).Strings[Idx - 1];
      end
      else
        Str := IntTostr(Idx);

      StringGrid1.Cells[Idx + 1, 0] := Str + '( ' + GetTypeString(Byte(TParamItem(FObjList[0]).FFieldsList[Idx - 1])) + ' )'  ;
      StringGrid1.ColWidths[Idx + 1]  := FMinColSz;
    end;

    FilterList := TStringList.Create;
    FilterList.Delimiter := ';';
    FilterList.DelimitedText := FFilter;
    if FilterList.Count > 0 then
    begin
      StringGrid1.RowCount := FilterList.Count + 1;
      for Idx := 0 to FilterList.Count - 1 do
      if TryStrToInt(FilterList[Idx], valInt) then
        if (valInt >= 0) and (FObjList.Count > valInt) then
        begin
          par := TParamItem(FObjList[valInt - 1]);
          StringGrid1.Cells[0, Idx + 1] := par.FparamName.ParamData[0] + '(' + IntToStr(par.FIndex) + ')' + '(' + IntToStr(Idx) + ')';
          StringGrid1.ColWidths[0] := 200;
          StringGrid1.Cells[1, Idx + 1] := par.FparamName.ParamData[0];
          StringGrid1.ColWidths[1] := 200;
          for Idx2 := 0 to par.FItemsList.Count - 1 do
          begin
            Str := UpdTextInCell(par, Idx2, valInt - 1);
            Ln := Length(Str);
            ln := ln * 10;
            if ln > 150 then ln := 150;
            if StringGrid1.ColWidths[Idx2 + 2] < ln  then
              StringGrid1.ColWidths[Idx2 + 2] := ln;
            StringGrid1.Cells[Idx2 + 2, Idx + 1 ] := Str;
         end;
        end;

      FilterList.Free;
      exit;
    end;
    FilterList.Free;
    StringGrid1.RowCount := FObjList.Count + 1;
    for Idx := 0 to FObjList.Count - 1 do
      begin
        par := TParamItem(FObjList[Idx]);
        StringGrid1.Cells[0, Idx + 1] := par.FparamName.ParamData[0] + '(' + IntToStr(par.FIndex) + ')'  + '(' + IntToStr(Idx) + ')';
        StringGrid1.ColWidths[0] := 200;
        StringGrid1.Cells[1, Idx + 1] := par.FparamName.ParamData[0];
        StringGrid1.ColWidths[0] := 200;
        for Idx2 := 0 to par.FItemsList.Count - 1 do
        begin
          Str := UpdTextInCell(par, Idx2, Idx);
          Ln := Length(Str);
          ln := ln * 10;
          if ln > 150 then ln := 150;
          if StringGrid1.ColWidths[Idx2 + 2] < ln  then
            StringGrid1.ColWidths[Idx2 + 2] := ln;
          StringGrid1.Cells[Idx2 + 2, Idx + 1] := Str;
         end;
      end;
  end
//  else if(Item.Data <> nil) and (TObject(Item.Data) is TMemoryStream) and (TObject(Item.Data) <> FCurItem)  then
//  begin
//    FCurItem := TMemoryStream(Item.Data);;
//    FillDefineStringGrid;
//  end
//  else if(Item.Data <> nil) and (TObject(Item.Data) is TStringList) and (TObject(Item.Data) <> FCurItem)  then
//  begin
//    FCurItem := TStringList(Item.Data);;
//    FillObjNumStringGrid;
//  end;



end;

procedure TForm1.LEObjListResize(Sender: TObject);
begin
if LEObjList.Columns.Count > 0 then
  LEObjList.Columns[0].Width := LEObjList.Width - 4;
end;

procedure TForm1.ReadDefines(Stream: TStream; Stream2: TStream);
var
  DWRead: DWORD;
begin
    Stream.Read(DWRead, 4);
    FDefinesStream.SetSize(DWRead);
    FDefinesStream.CopyFrom(Stream, DWRead);

//    if Stream2 <> nil then
//    begin
//      Stream2.Read(DWRead, 4);
//      FDefinesStream.Position := FDefinesStream.Size;
//      FDefinesStream.SetSize(FDefinesStream.Size + DWRead);
//      FDefinesStream.CopyFrom(Stream2, DWRead);
//    end;
end;

procedure TForm1.SetEditTextToCell(Text: String; ACol, ARow: Integer);
var
  lst: TList;
  par: TCustomParam;
  typ: Integer;
  ShowTyp: Integer;
  valInt: Integer;
  valSingle: Single;
  Idx, i: Integer;
  StrList: TStringList;
  NumList: TList;
  AStr: AnsiString;
  Str: String;
  Err: Boolean;
  sint: SmallInt;
  DW: DWORD;
  Vi64: Int64;
begin
  if (FCurItem is TList) and (ACol = 0) then
  begin
    lst := TList(FCurItem);
    TParamItem(lst[ARow - 1]).FParamName.ParamData[0] := Text;
    FConstNameList[TParamItem(lst[ARow - 1]).FIndex] := Text;
    Exit;
  end;
  if FCurItem is TList then
  begin
    lst := TList(FCurItem);
    par := TCustomParam(TParamItem(lst[ARow - 1]).FItemsList[ACol - 1]);
    typ := Integer(TParamItem(lst[ARow - 1]).FFieldsList[ACol - 1]);
    ShowTyp := Integer(TParamItem(lst[ARow - 1]).FFileldsNames.Objects[ACol - 1]);
    Err := False;
    if (typ in [0, 1, 2, 3, 4]) then
    begin
      if (ShowTyp = 5) and (length(Text) <= 8) then
      begin
        SetLength(AStr, 8);
        Str := Text;
        for i := 1 to 8 - length(Text) do
          Str := '0' + Str;
        ZeroMemory(@AStr[1], 8);
        if HexToBin(PWideChar(@Str[1]), PAnsiChar(@AStr[1]), length(Str)) = length(Str) div 2 then
        begin
          DW := (BYTE(AStr[1]) shl 24) + (BYTE(AStr[2]) shl 16) + (BYTE(AStr[3]) shl 8) + (BYTE(AStr[4]));
          Text := IntToStr(DW);
        end
        else
        begin
          MessageDlg('Incorrect hex value.', mtError, [mbOK], 0);
          Exit;
        end;
      end
      else if (ShowTyp = 5) then
        MessageDlg('Incorrect hex value.', mtError, [mbOK], 0);

      if (typ in [0, 1, 2]) then
      begin
        if (TryStrToInt64(Text, Vi64)) then
        begin
          valInt := Vi64;
          if (typ = 0) then
          begin
            if (valInt >=0) and (valInt <= 255)  then
              par.ParamData[0] := valInt
            else
              MessageDlg('Incorrect value, should be between 0..255.', mtError, [mbOK], 0);
          end
          else if (typ = 1) then
          begin
            if ((valInt >=0) and (valInt <= 65535)) or ((valInt >=-32767) and (valInt <= 32768))  then
            begin
              sint := valInt;
              par.ParamData[0] := sint;
            end
            else
              MessageDlg('Incorrect value, should be between 0..65535 or -32767..32768.', mtError, [mbOK], 0);
          end else if (typ = 2) then
            par.ParamData[0] := valInt;
        end
        else
          MessageDlg('Incorrect Integer value.', mtError, [mbOK], 0);
      end
      else if (typ  = 3)then
      begin
        if (TryStrToFloat(Text, valSingle)) then
          par.ParamData[0] := valSingle
        else
          MessageDlg('Incorrect float value.', mtError, [mbOK], 0);
      end
      else if (typ  = 4)  then
      begin
        if (TryStrToInt(Text, valInt)) then
        begin
            if (valInt >= 0) and (valInt < FConstNameList.Count) then
            begin
              valInt := Integer(FConstNameList.Objects[valInt]);
              par.ParamData[0] := valInt;
            end
            else
            if (valInt >= 0) and (valInt - FConstNameList.Count < FObjRefList.Count) then
            begin
              valInt := Integer(FObjRefList.Objects[valInt - FConstNameList.Count]);
              par.ParamData[0] := valInt;
            end
            else
              MessageDlg('Incorrect index value.', mtError, [mbOK], 0);
        end
        else
        begin
          valInt := FConstNameList.IndexOf(Text);
          if valInt <> - 1 then
          begin
            valInt := Integer(FConstNameList.Objects[valInt]);
            par.ParamData[0] := valInt;
          end
          else
          begin
            valInt := FObjRefList.IndexOf(Text);
            if valInt <> - 1 then
            begin
              valInt := Integer(FObjRefList.Objects[valInt]);
              par.ParamData[0] := valInt;
            end
            else
              MessageDlg('Incorrect constant name.', mtError, [mbOK], 0);
          end;
        end;

      end
      else if (typ  = 5) then
      begin
        if (TryStrToInt64(Text, Vi64)) then
        begin
///           valInt := Vi64;
        if Vi64 >= Form1.FDefinesStream.Size then
        begin
          MessageDlg('Incorrect constant value.', mtError, [mbOK], 0);
          Exit;
        end;
          par.ParamData[0] := DWORD(Vi64);
        end
        else
          MessageDlg('Incorrect Integer value.', mtError, [mbOK], 0);
      end;
    end
    else if (typ = 5) then
    begin
      par.ParamData[0] := Text;
    end
    else
    begin

      StrList := TStringList.Create;
      StrList.Delimiter := ';';
      StrList.DelimitedText := Text;
      NumList := TList.Create;
      for Idx := 0 to StrList.Count - 1 do
      begin
        if (typ in [7, 8, 9]) then
        begin
          if (TryStrToInt(StrList[Idx], valInt)) then
          begin
            if (typ = 7) then
            begin
              if (valInt >=0) and (valInt <= 255)  then
                NumList.Add( TObject(valInt))
              else
              begin
                MessageDlg('Incorrect set value, should be between 0..255.', mtError, [mbOK], 0);
                Err := True;
                break;
              end;
            end
            else if (typ = 8) then
            begin
              if (valInt >=0) and (valInt <= 65535)  then
                NumList.Add( TObject(valInt))
              else
              begin
                MessageDlg('Incorrect set value, should be between 0..65535.', mtError, [mbOK], 0);
                Err := True;
                break;
              end;
            end
            else if (typ = 9) then
              NumList.Add( TObject(valInt));
          end
          else
          begin
            MessageDlg('Incorrect Integer value in the set.', mtError, [mbOK], 0);
            Err := True;
            break;
          end;
        end
        else if typ = 10 then
        begin
          if (TryStrToFloat(StrList[Idx], valSingle)) then
            NumList.Add( TObject(valSingle))
          else
          begin
            MessageDlg('Incorrect float value in the set.', mtError, [mbOK], 0);
            Err := True;
            break;
          end;
        end
        else if typ = 11 then
        begin
          if (TryStrToInt(StrList[Idx], valInt)) then
          begin
            if (valInt >= 0) and (valInt < FConstNameList.Count) then
            begin
              valInt := Integer(FConstNameList.Objects[valInt]);
              NumList.Add( TObject(valInt));
            end
            else if (valInt >= 0) and (valInt - FConstNameList.Count < FObjRefList.Count) then
            begin
              valInt := Integer(FObjRefList.Objects[valInt - FConstNameList.Count]);
              NumList.Add( TObject(valInt));
            end
            else
            begin
              MessageDlg('Incorrect index value in the set.', mtError, [mbOK], 0);
              Err := True;
              break;
            end;
          end
          else
          begin
            valInt := FConstNameList.IndexOf(StrList[Idx]);
            if valInt <> - 1 then
            begin
              valInt := Integer(FConstNameList.Objects[valInt]);
              NumList.Add( TObject(valInt));
            end
            else
            begin
              valInt := FObjRefList.IndexOf(StrList[Idx]);
              if valInt <> - 1 then
              begin
                valInt := Integer(FObjRefList.Objects[valInt]);
                NumList.Add( TObject(valInt));
              end
              else
              begin
                MessageDlg('Invalid constant name in the set.', mtError, [mbOK], 0);
                Err := True;
                break;
              end;
            end;
          end;
        end
        else if typ = 12 then
        begin
        // do nothing
        end
      end;

      if not Err then
      begin
        par.Clear;
        if typ = 12 then
        begin
          for Idx := 0 to StrList.Count - 1 do
            par.ParamData[-1] := StrList[idx];
        end
        else
        begin
          for Idx := 0 to NumList.Count - 1 do
            if typ = 10 then
              par.ParamData[-1] := Single(NumList[Idx])
            else
              par.ParamData[-1] := Integer(NumList[Idx])
        end;
      end;

      NumList.Free;
      StrList.Free;
    end;
  end;
end;

procedure TForm1.Showtypes1Click(Sender: TObject);
Var
  Item: TObject;
begin
  if (FAhCol < 2) or (FAhRow = -1) then Exit;
  if (LEObjList.ItemIndex >= FFileldsNames.Count) or (LEObjList.ItemIndex < 0) then Exit;
  if not(TObject(FFileldsNames[LEObjList.ItemIndex]) is TStringList) then  Exit;
  TStringList(FFileldsNames[LEObjList.ItemIndex]).Objects[FAhCol - 2] := TObject(TComponent(Sender).Tag);
  Item := FCurItem;
  FCurItem := nil;
  LEObjListChange(Self, LEObjList.Items[LEObjList.ItemIndex], TItemChange(nil));
  if FCurItem = nil then
    FCurItem := Item;

end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
var
  I: Integer;
  s1, s2 : String;
  aKey: Char;
begin
  if FindALLCB.Checked then
  begin
    FindAll(Edit3.Text, Memo1.Lines);
    Exit;
  end;
  if FLastFound >= StringGrid1.RowCount - 1  then FLastFound := 1;
  for I := FLastFound to StringGrid1.RowCount - 1 do
  begin
    if CheckBox2.Checked then
    begin
      s1 := Edit3.Text;
      s2 := StringGrid1.Cells[FACol, I];
    end
    else
    begin
      s1 := UpperCase(Edit3.Text);
      s2 := UpperCase(StringGrid1.Cells[FACol, I]);
    end;
    if Pos(s1, s2) > 0 then
    begin
      StringGrid1.Row := i;
      if CheckBox1.Checked then
      begin
        FIsEditing := True;
        FEditTexst := Edit1.Text;
        aKey := Char(VK_RETURN);
        StringGrid1KeyPress(nil, aKey);
        FEditTexst := '';
        FIsEditing := False;
      end;
      FLastFound := I + 1;
      if not (CheckBox3.Checked and CheckBox1.Checked) then
        Exit;
    end;
  end;
  FLastFound := 1;
end;

procedure TForm1.Splitter1Paint(Sender: TObject);
begin
 Splitter1.Canvas.Draw(0, (Splitter1.Height - Image1.Height) div 2, Image1.Picture.Bitmap);
end;

procedure TForm1.StringGrid1Click(Sender: TObject);
begin
  if FIsEditing then
  begin
    StringGrid1.Cells[FACol, FARow] := FOldEditText;
    FIsEditing := False;
  end;
end;

procedure TForm1.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
if (ACol = 0) and (ARow = 0) then
begin
   BtnDwnLst.Left := StringGrid1.Left + Rect.Right - BtnDwnLst.Width;
   BtnDwnLst.Top := StringGrid1.Top + Rect.Bottom - BtnDwnLst.Height;
end;
//if (ACol = 0) then
//begin
//StringGrid1.Canvas.Brush.Style := bsSolid;
 // StringGrid1.Canvas.Brush.Color := $0046DAFF;
///  StringGrid1.Canvas.FillRect(Rect);
//end
//else
  //StringGrid1.Canvas.Brush.Color := $009BEBFF;
///  StringGrid1.Color := $009BEBFF;
  //StringGrid1.Canvas.Brush.Color := $009BEBFF;
end;

procedure TForm1.StringGrid1GetEditText(Sender: TObject; ACol, ARow: Integer;
  var Value: string);
begin
   FOldEditText := Value;
   if ACol <> 0 then
     Value := GetEditTextFromCell(ACol - 1, ARow);
end;

procedure TForm1.StringGrid1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = VK_F3 then
    SpeedButton1Click(Self);
end;

procedure TForm1.StringGrid1KeyPress(Sender: TObject; var Key: Char);
var
  FilterList : TStringList;
  ARow: Integer;
begin
  if (Key = Char(VK_RETURN))  and  FIsEditing then
  begin

    FilterList := TStringList.Create;
    FilterList.Delimiter := ';';
    FilterList.DelimitedText := FFilter;
    if FilterList.Count > 0 then
    begin
      //if FACol = 0 then
      //  StringGrid1.Cells[FACol, FARow] := FOldEditText
      //else
      begin
        ARow := StrToInt(FilterList[FARow - 1]);
        SetEditTextToCell(FEditTexst, FACol - 1, ARow);
        StringGrid1.Cells[FACol, FARow] := GetEditTextFromCell(FACol - 1, FARow, False);
        StringGrid1.Invalidate;
      end;
      FOldEditText := StringGrid1.Cells[FACol, FARow];
      FIsEditing := False;

      FilterList.Free;
      Exit;
    end;
    FilterList.Free;



   //if FACol = 0 then
//      StringGrid1.Cells[FACol, FARow] := FOldEditText
  // else
   begin
      SetEditTextToCell(FEditTexst, FACol - 1, FARow);
      StringGrid1.Cells[FACol, FARow] := GetEditTextFromCell(FACol - 1, FARow, False);
      StringGrid1.Invalidate;
   end;
   FOldEditText := StringGrid1.Cells[FACol, FARow];
   FIsEditing := False;
  end;
end;

procedure TForm1.StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight  then
    FIsMBtnDown := True;
end;

procedure TForm1.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Longint;
  typ, i: Integer;
begin
  if FIsMBtnDown then
  begin
    FIsMBtnDown := False;
    StringGrid1.MouseToCell(X, Y, ACol, ARow);
    if ARow = 0 then
    begin
      FAhCol := ACol;
      FAhRow := ARow;
      if FAhCol < 2 then Exit;
      if (LEObjList.ItemIndex >= FFileldsNames.Count) or (LEObjList.ItemIndex < 0) then Exit;
      if not(TObject(FFileldsNames[LEObjList.ItemIndex]) is TStringList) then  Exit;
      i := Integer(TStringList(FFileldsNames[LEObjList.ItemIndex]).Objects[FAhCol - 2]);
      typ := GetColumnType(FAhCol - 1, FAhRow);
      if Typ in [0, 1, 2, 3, 5, 7, 8, 9, 10]  then
      begin
        PEditMenu.Items[6].Visible := True;
      end
      else
        PEditMenu.Items[6].Visible := False;

      if Typ in [0, 1, 2, 3] then
      begin
        PEditMenu.Items[1].Visible := False;
        PEditMenu.Items[2].Visible := False;
        PEditMenu.Items[3].Visible := True;
        PEditMenu.Items[4].Visible := True;
        PEditMenu.Items[5].Visible := True;
        if I = 0 then
          I := 3;
      end
      else if Typ = 4 then
      begin
        PEditMenu.Items[1].Visible := True;
        PEditMenu.Items[2].Visible := True;
        PEditMenu.Items[3].Visible := False;
        PEditMenu.Items[4].Visible := False;
        PEditMenu.Items[5].Visible := False;
        if I = 0 then
          I := 1;
      end
      else
      begin
        PEditMenu.Items[1].Visible := False;
        PEditMenu.Items[2].Visible := False;
        PEditMenu.Items[3].Visible := False;
        PEditMenu.Items[4].Visible := False;
        PEditMenu.Items[5].Visible := False;
        I := 0;
      end;

      CheckItem(I);
      PEditMenu.Popup(Self.Left + StringGrid1.Left + X, Self.Top + ToolBar1.Height + StringGrid1.Top + Y);
    end
    else
    begin
      FAhCol := -1;
      FAhRow := -1;
      PMGrid.Popup(Self.Left + StringGrid1.Left + X, Self.Top + ToolBar1.Height + StringGrid1.Top + Y);
    end;
  end;
end;

procedure TForm1.StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  if not FIsEditing then
  begin
    FACol := ACol;
    FARow := ARow;
  end;
//if ACol = 0 then
//begin
//   StringGrid1.Options := StringGrid1.Options - [goEditing];
//end
//else
//StringGrid1.Options := StringGrid1.Options + [goEditing];

end;

procedure TForm1.StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin
 FIsEditing := True;
//if ACol <> 0 then
begin
 FEditTexst := Value;
end;
 FARow := ARow;
 FACol := ACol;
end;

procedure TForm1.TBOpenClick(Sender: TObject);
var
  DWRead: DWORD;
  Idx, ItemIdx, PrevIdx: Integer;
  ParamItem: TParamItem;
  FObjList: TList;
  GName: String;
//  FFile2: TFileStream;
begin
  OpenDialog1.Title := 'Select a file';
  if FFileNameA = '' then
    if not OpenDialog1.Execute() then
      Exit;

    if FFileNameA = '' then
      FFile := TFileStream.Create(OpenDialog1.FileName, 0)
    else if FileExists(FFileNameA) then
      FFile := TFileStream.Create(FFileNameA, 0)
    else
      Exit;
    StatusBar1.Panels.Items[1].Text := 'Loading...';
  LEObjList.Items.BeginUpdate;
  try
    FFile.Read(DWRead, 4);
    if (DWRead <> $00015250) and (DWRead <> $0005250) then
    begin
      //LBErrorLog.AddItem('Couldn''''t open: Incorrect file type', nil);
      MessageDlg('Couldn''''t open: Incorrect file type', mtError, [mbOK], 0);
      Exit;
    end;
    FVer := DWRead;
    Clear;
    FObjList := nil;
    Memo1.Clear;
    FFile.Read(FDW1, 4);
    FFile.Read(FDW2, 4);
    FFile.Read(GUID, 16);
    ReadDefines(FFile, nil);
    //LEObjList.AddItem('Defines', FDefinesStream);
    LEObjList.ViewStyle := vsReport;
    LEObjList.Columns.Clear;
    LEObjList.Items.Clear;
    with LEObjList.Columns.Add do
        begin
          Index := 0;
          Caption := 'Name';
          Width := 200;
        end;
    FFile.Read(FSize, 4);
    FFile.Read(FNewSize, 4);
    FFile.Read(FDW3, 4);
    ProgressBar1.Min := 0;
    ProgressBar1.Max := FSize;
    ProgressBar1.Step := 1;
    StringGrid1.Enabled := False;
    LEObjList.Enabled := False;
    ToolBar1.Enabled := False;
    for Idx := 0 to FSize - 1 do
    begin
      ParamItem := TParamItem.Create;
      FConstNameList.AddObject('', ParamItem);
    end;
    for Idx := 0 to FSize - 1 do
    begin
//      ParamItem := TParamItem.Create;
      ProgressBar1.StepIt;
      Application.ProcessMessages;
      ParamItem := TParamItem(FConstNameList.Objects[Idx]);
      PrevIdx := Flist.Count;
      ParamItem.ReadFromStream(FFile, FDefinesStream, FDefinesList, FConstNameList, FList, FFileldsNames, FObjRefList);
      ParamItem.FIndex := Idx;
      FConstNameList.Strings[Idx] := ParamItem.FparamName.ParamData[0];
//      FConstNameList.AddObject(ParamItem.FparamName.ParamData[0], ParamItem);
      if PrevIdx < Flist.Count then
      begin
        FObjList := TList.Create;
        LEObjList.AddItem(ParamItem.FParamName.ParamData[0], FObjList);
      end;
      if FObjList <> nil then
        FObjList.Add(ParamItem);
    end;

     if FFile.Position < FFile.Size  then
     begin
       FFile.Read(DWRead, 4);
       FFile.Read(DWRead, 4);
     end;
     Idx := 0;
     while FFile.Position < FFile.Size do
     begin
       if Idx < FObjRefList.Count  then
         TParamItem(FObjRefList.Objects[Idx]).FParamName.ReadFromStream(FFile)
       else
       begin
         FObjRefList.AddObject(IntToStr(Idx), TParamItem.Create);
         TParamItem(FObjRefList.Objects[Idx]).FIsImported := True;
         TParamItem(FObjRefList.Objects[Idx]).FParamName.ReadFromStream(FFile);
       end;
       FObjRefList.Strings[Idx] := TParamItem(FObjRefList.Objects[Idx]).FParamName.ParamData[0];
       TParamItem(FObjRefList.Objects[Idx]).FIndex := Idx;
       Inc(Idx);
     end;

     if FDW2 > 1 then
       GName := 'GROUPS NAMES' + IntToStr(FDW2)
     else
       GName := 'GROUPS NAMES';

     if FVer = $0005250 then
      GName := 'RC ' + GName;


     for Idx := 0 to LEObjList.Items.Count - 1 do
      LEObjList.Items[Idx].Caption := FIniSettings.ReadString (GName, IntToStr(Idx), LEObjList.Items[Idx].Caption);

     for Idx := 0 to FFileldsNames.Count - 1 do
       if TObject(FFileldsNames[Idx]) is TStringList then
         for ItemIdx := 0 to TStringList(FFileldsNames[Idx]).Count - 1 do
            TStringList(FFileldsNames[Idx]).Strings[ItemIdx] := FIniSettings.ReadString (IntToStr(Idx), IntToStr(ItemIdx), TStringList(FFileldsNames[Idx]).Strings[ItemIdx]);

    //LEObjList.AddItem('Items numbers', FConstNameList);
    LEObjList.ItemIndex := 1;
    ToolButton2.Enabled := True;
    ToolButton3.Enabled := True;
    ToolButton10.Enabled := True;
    ToolButton13.Enabled := True;
  finally
    LEObjList.Items.EndUpdate;
    FFile.Free;
    //FFile2.Free;
    ProgressBar1.Position := 0;
    StatusBar1.Panels.Items[1].Text := '';
    StringGrid1.Enabled := True;
    LEObjList.Enabled := True;
    ToolBar1.Enabled := True;
  end;
end;

procedure TForm1.ToolButton10Click(Sender: TObject);
var
  StrLst: TStringList;
  I, valInt: Integer;
  CurItem: TObject;
begin
  FFilter := InputBox('Filter edit', 'Enter rows numbers, separate by ";" :', FFilter);
  StrLst := TStringList.Create;
  StrLst.Delimiter := ';';
  StrLst.DelimitedText := FFilter;
  for I := 0 to StrLst.Count - 1 do
    if not TryStrToInt(StrLst[I], valInt) then
    begin
      FFilter := '';
      MessageDlg('Invalid value in the filter set.', mtError, [mbOK], 0);
      break;
    end;
  CurItem := FCurItem;
  FCurItem := nil;
  LEObjListChange(nil, LEObjList.Items[LEObjList.ItemIndex], TItemChange(0));
  FCurItem := CurItem;
  StrLst.Free;
end;

procedure TForm1.ToolButton12Click(Sender: TObject);

begin
//  ZeroMemory(@sinf, sizeof(sinf));
//  sinf.cb := sizeof(sinf);
//  sinf.dwFlags := STARTF_USESHOWWINDOW;
//  sinf.wShowWindow := SW_SHOWNORMAL;
//  CreateProcess('H:\Games\Dva Mira II\DvaMira2.exe', '', nil, nil, false, NORMAL_PRIORITY_CLASS, 0, 'H:\Games\Dva Mira II\', sinf, pinf);

//NORMAL_PRIORITY_CLASS
//
//CALL 0040A3A0
//CALL 004058D0
//004ACFB0
end;

procedure TForm1.ToolButton13Click(Sender: TObject);
{var
    id: DWORD;
  hThread: Cardinal;
  }
begin
// 	hThread := CreateRemoteThread(
//					pinf.hProcess,
//					0,
//					0,
//					Pointer($0040A3A0),
//					0,
//					0,
//					&id);
end;

procedure TForm1.ToolButton2Click(Sender: TObject);
var
  DWWrite: DWORD;
  ParamItem: TParamItem;
  Idx: Integer;
  Mem: TMemoryStream;
  SaveIdx: Integer;
begin
  if FFileNameA = '' then
    if not SaveDialog1.Execute() then
      Exit;

  begin
    if FFileNameA <> '' then
      FFile := TFileStream.Create(FFileNameA, fmCreate)
    else
      FFile := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    StatusBar1.Panels.Items[1].Text := 'Saving...';
    try
      FDefinesStream.Clear;
      FDefinesStream.Size := 0;
      FDefinesStream.Position := 0;
      FDefinesList.Clear;
      DWWrite := FVer;// $00015250;
      FFile.Write(DWWrite, 4);
      FFile.Write(FDW1, 4);
      FFile.Write(FDW2, 4);
      FFile.Write(GUID, 16);

      Mem := TMemoryStream.Create;
      ProgressBar1.Min := 0;
      ProgressBar1.Max := FSize;
      StringGrid1.Enabled := False;
      LEObjList.Enabled := False;
      ToolBar1.Enabled := False;
      for Idx := 0 to FSize - 1 do
      begin
        ParamItem := TParamItem(FConstNameList.Objects[Idx]);
        ParamItem.FIndex := Idx;
       // ParamItem.FParamName.ParamData[0] := FConstNameList[Idx];
      end;

      SaveIdx := FConstNameList.Count;
      FObjRefList.Clear;
      for Idx := 0 to FSize - 1 do
      begin
        ProgressBar1.StepIt;
        Application.ProcessMessages;
        ParamItem := TParamItem(FConstNameList.Objects[Idx]);
        ParamItem.WriteToStream(Mem, FDefinesStream, FDefinesList, FConstNameList, FObjRefList, SaveIdx);
      end;

      WriteDefines(FFile);
      FFile.Write(FSize, 4);
      FFile.Write(FNewSize, 4);
      FFile.Write(FDW3, 4);
      Mem.Position := 0;
      FFile.CopyFrom(Mem, Mem.Size);
      Mem.Free;
      DWWrite := FObjRefList.Count;
      FFile.Write(DWWrite, 4);
      DWWrite := 0;
      FFile.Write(DWWrite, 4);
      for Idx := 0 to FObjRefList.Count - 1 do
      begin
        Application.ProcessMessages;
        ParamItem := TParamItem(FObjRefList.Objects[Idx]);
        ParamItem.FParamName.WriteToStream(FFile);
        ParamItem.FIndex := Idx;
      end;
      FDefinesStream.Clear;
      FDefinesStream.Size := 0;
      FDefinesStream.Position := 0;
      FDefinesList.Clear;
    finally
      FFile.Free;
      ProgressBar1.Position := 0;
      StatusBar1.Panels.Items[1].Text := 'Saved';
      StringGrid1.Enabled := True;
      LEObjList.Enabled := True;
      ToolBar1.Enabled := True;
    end;
  end;
end;

procedure TForm1.ToolButton3Click(Sender: TObject);
begin
  ConstantEditor.Form2.Show;
  ConstantEditor.Form2.SetConstantsList(FConstNameList, FDefinesStream, FObjRefList);
end;

procedure TForm1.ToolButton4Click(Sender: TObject);
begin
  with TForm3.Create(Self) do
  begin
    FValue := FMinColSz;
    if ShowModal = mrOK then
    begin
      FMinColSz := FValue;
    end;
    Free;
  end;
end;

procedure TForm1.ToolButton8Click(Sender: TObject);
var
 i, j: Integer;
 GName: String;
begin
  if FDW2 > 1 then
    GName := 'GROUPS NAMES' + IntToStr(FDW2)
  else
    GName := 'GROUPS NAMES';

  if FVer = $0005250 then
    GName := 'RC ' + GName;

  for i := 0 to LEObjList.Items.Count - 1 do
     FIniSettings.WriteString(GName, IntToStr(i), LEObjList.Items[i].Caption);
  for i := 0 to FFileldsNames.Count - 1 do
    if TObject(FFileldsNames[i]) is TStringList then
      for j := 0 to TStringList(FFileldsNames[i]).Count - 1 do
        FIniSettings.WriteString (IntToStr(i), IntToStr(j), TStringList(FFileldsNames[i]).Strings[j]);

  FIniSettings.WriteString('Options', 'MixColWidth', IntToStr(FMinColSz));
end;


procedure TForm1.WriteDefines(Stream: TStream);
var
  DWWrite: DWORD;
begin
///FDefinesList
  DWWrite := FDefinesStream.Size;
  Stream.Write(DWWrite, 4);
  FDefinesStream.Position := 0;
  Stream.CopyFrom(FDefinesStream, DWWrite);
end;

{ TParamItem }

procedure TParamItem.AssignAll(Source: TParamItem; StringStream: TMemoryStream; StringList, AConstNameList, BConstNameList,  FixUpList, FObjRefList: TStringList; ClearValues: Boolean = False);
var
  Idx, Idy: Integer;
  Index1, LastParIdx: Integer;
  par, par2: TCustomParam;
  NewPar: TParamItem;
begin
  FParamName.ParamData[0] := Source.FParamName.ParamData[0];

//      FParamData: TObject;
  FSectionCount := Source.FSectionCount;
  FRowCount := Source.FRowCount;
  FSize := Source.FSize;
  FFieldsList := Source.FFieldsList;
  FIsDefinesParam := Source.FIsDefinesParam;
  bByte := Source.bByte;
  bNewFld := 1;
  wColCount := Source.wColCount;
  wSize := Source.wSize;
  wnSize := Source.wnSize;
  FIndex := Source.FIndex;
  FFileldsNames := Source.FFileldsNames;
  for Idx := 0 to Source.FItemsList.Count - 1 do
  begin
    par2 := TCustomParam(Source.FItemsList[Idx]);
//    ClassRef := par2.ClassType;
//    par := TCustomParam(ClassRef.Create);
    if Byte(FFieldsList[Idx]) = 2 then
      par := TIntegerParam.Create
    else if Byte(FFieldsList[Idx]) = 3 then
      par := TFloatParam.Create
    else if Byte(FFieldsList[Idx]) = 4 then
      par := TIntegerParam.Create(True)
    else if Byte(FFieldsList[Idx]) = 5 then
      par := TStringParam.Create(StringStream, StringList)
    else if Byte(FFieldsList[Idx]) = 7 then
      par := TByteParamSet.Create
    else if Byte(FFieldsList[Idx]) = 8 then
      par := TWordParamSet.Create
    else if Byte(FFieldsList[Idx]) = 9 then
      par := TIntegerParamSet.Create
    else if Byte(FFieldsList[Idx]) = 10 then
      par := TFloatParamSet.Create
    else if Byte(FFieldsList[Idx]) = 11 then
      par := TIntegerParamSet.Create(True)
    else if Byte(FFieldsList[Idx]) = 12 then
      par := TStringParamSet.Create
    else if Byte(FFieldsList[Idx]) = 0 then
      par := TByteParam.Create
    else if Byte(FFieldsList[Idx]) = 1 then
      par := TWordParam.Create
    else
      par := TIntegerParam.Create;

    LastParIdx := 0;
    for Idy := 0 to par2.Count - 1 do
    begin
      if (par2.IsAddrSored) then
      begin
        //Index1 := BConstNameList.IndexOfObject(TObject(DWORD(par2.ParamData[Idy])));
        //if (Index1 <> - 1
        if {(Index1 <> - 1) and} not ClearValues then
        begin
          Index1 := AConstNameList.IndexOf(TParamItem(DWORD(par2.ParamData[Idy])).FParamName.ParamData[0]);
          if Index1 <> -1  then
            par.ParamData[-1] := DWORD(AConstNameList.Objects[Index1])
          else
          begin
            Index1 := FObjRefList.IndexOf(TParamItem(DWORD(par2.ParamData[Idy])).FParamName.ParamData[0]);
            if Index1 <> -1 then
              par.ParamData[-1] := DWORD(FObjRefList.Objects[Index1])
            else
            begin
              if FixUpList = nil then continue;
              
              if TParamItem(DWORD(par2.ParamData[Idy])).FIsImported then
              begin
                NewPar := TParamItem.Create;
                NewPar.FParamName.ParamData[0] := TParamItem(DWORD(par2.ParamData[idy])).FParamName.ParamData[0];
                NewPar.FIndex := FObjRefList.Count;
                NewPar.FIsImported := True;
                FObjRefList.AddObject(NewPar.FParamName.ParamData[0], NewPar);
              end;

              Index1 :=  FixUpList.IndexOf(Source.FparamName.ParamData[0]);
              if Index1 = -1 then
              begin
                FixUpList.AddObject(Source.FparamName.ParamData[0], TList.Create);
                Index1 := FixUpList.Count - 1;
              end;
              if LastParIdx = 0 then
              begin
                TList(FixUpList.Objects[Index1]).Add(TObject(Idx));
                inc(LastParIdx);
              end;
            end;
          end;
         // par.ParamData[-1] := DWORD(AConstNameList.Objects[0]);
        end;
      end
      else
      begin
        if ClearValues then
        begin
          if (par is TStringParam) or (par is TStringParamSet) then
            par.ParamData[-1] := ''
          else
            par.ParamData[-1] := 0
        end
        else
          par.ParamData[-1] := par2.ParamData[Idy];
      end;
    end;
    FItemsList.Add(par);
  end;
end;

constructor TParamItem.Create;
begin
  FParamName := TStringParam.Create(nil, nil);
  FItemsList := TList.Create;
  FIsImported := False;
end;

destructor TParamItem.Destroy;
var
  Idx: Integer;
begin
  for idx := 0 to FItemsList.Count - 1 do
  if TObject(FItemsList[Idx]) is TObject then
  begin
    TObject(FItemsList[Idx]).Free
  end;

  FParamName.Free;
  FItemsList.Free;
  inherited;
end;

procedure TParamItem.ParseToListView(ListView: TListView);
var
  Idx, Idx2: Integer;
  Itm: TlistItem;
  Str: String;
begin

   // if FParamData = nil then Exit;
    ListView.ViewStyle := vsReport;
    if FParamData is TStringList then
    begin
      with TStringList(FParamData) do
      begin
        with ListView.Columns.Add do
        begin
          Index := 0;
          Caption := 'Idx';
        end;
        with ListView.Columns.Add do
        begin
          Index := 1;
          Caption := 'Define string';
          Width := 200;
        end;
        for Idx:= 0 to Count - 1 do
        with ListView.Items.Add do
        begin
          Caption := IntTostr(Integer(Objects[Idx]));
          SubItems.Add(Strings[Idx]);
        end;
      end;
      Exit;
    end;


        Itm := ListView.Items.Add;
        Itm.Caption := FparamName.ParamData[0];
        for Idx:= 0 to FItemsList.Count - 1 do
        begin

          if TCustomParam(FItemsList[Idx]).Count > 0 then
            Str := TCustomParam(FItemsList[Idx]).ParamData[0];
          for Idx2 := 1 to TCustomParam(FItemsList[Idx]).Count - 1 do
          if TCustomParam(FItemsList[Idx]) is TFloatParam then
            Str := Str + ';' + FloatToStr(TCustomParam(FItemsList[Idx]).ParamData[Idx2])
          else
            Str := Str + ';' + varToStr(TCustomParam(FItemsList[Idx]).ParamData[Idx2]);

          Itm.SubItems.Add(Str);
        end;
end;

procedure TParamItem.ReadFromStream(Stream: TStream; Defines: TMemoryStream ;
  StrList: TStringList; ObjList: TStringList; List: TList; FileldsNames: TList; AddObjList: TStringList);
var
  Idx, Idy: Integer;
  SLst: TStringList;
  Lst: TList;
  bt: Byte;
  lItem: TCustomParam;
begin

  FParamName.ReadFromStream(Stream);
  Stream.Read(bByte, 1);
  Stream.Read(wColCount, 2);
  Stream.Read(wSize, 2);
  Stream.Read(wnSize, 2);
  Stream.Read(bNewFld, 1);

  if bNewFld = 0 then
  begin

    Lst := TList.Create;
    SLst := TStringList.Create;
    for Idx := 1 to wColCount do
    begin
      Stream.Read(bt, 1);
      Lst.Add(TObject(Integer(bt)));
      SLst.Add(IntToStr(Idx));
    end;
    List.Add(Lst);
    FileldsNames.Add(SLst);
  end;
  Lst := List[List.Count - 1];
  FFileldsNames := FileldsNames[FileldsNames.Count - 1];
  FFieldsList := Lst;
  for Idx := 0 to wColCount - 1 do
  begin
    if Byte(Lst[Idx]) = 2 then
      lItem := TIntegerParam.Create
    else if Byte(Lst[Idx]) = 3 then
      lItem := TFloatParam.Create
    else if Byte(Lst[Idx]) = 4 then
      lItem := TIntegerParam.Create(True)
    else if Byte(Lst[Idx]) = 5 then
      lItem := TStringParam.Create(Defines, StrList)
    else if Byte(Lst[Idx]) = 7 then
      lItem := TByteParamSet.Create
    else if Byte(Lst[Idx]) = 8 then
      lItem := TWordParamSet.Create
    else if Byte(Lst[Idx]) = 9 then
      lItem := TIntegerParamSet.Create
    else if Byte(Lst[Idx]) = 10 then
      lItem := TFloatParamSet.Create
    else if Byte(Lst[Idx]) = 11 then
      lItem := TIntegerParamSet.Create(True)
    else if Byte(Lst[Idx]) = 12 then
      lItem := TStringParamSet.Create
    else if Byte(Lst[Idx]) = 0 then
      lItem := TByteParam.Create
    else if Byte(Lst[Idx]) = 1 then
      lItem := TWordParam.Create
    else
      lItem := TIntegerParam.Create;

    lItem.ReadFromStream(Stream);
    if (Byte(Lst[Idx]) in [4, 11]) and (ObjList.Count > 0) then
    begin
      //lItem.ParamData[0] := DWORD(ObjList.Objects[lItem.ParamData[0]]);
      for Idy := 0 to lItem.Count - 1 do
        if lItem.ParamData[Idy] < ObjList.Count then
          lItem.ParamData[Idy] := DWORD(ObjList.Objects[lItem.ParamData[Idy]])
        else
        begin
          if lItem.ParamData[Idy] - ObjList.Count > AddObjList.Count - 1 then
            AddObjList.Capacity := lItem.ParamData[Idy] - ObjList.Count;
          if AddObjList.IndexOf(IntToStr(lItem.ParamData[Idy] - ObjList.Count)) = -1 then
            AddObjList.InsertObject(lItem.ParamData[Idy] - ObjList.Count,IntToStr(lItem.ParamData[Idy] - ObjList.Count), TParamItem.Create);
          lItem.ParamData[Idy] := DWORD(AddObjList.Objects[lItem.ParamData[Idy] - ObjList.Count]);
          TParamItem(DWORD(lItem.ParamData[Idy])).FIsImported := True;
        end;

    end;
    //for i := 0 to AddObjList.Count - 1 do
    FItemsList.Add(lItem);
  end;



end;


procedure TParamItem.WriteToStream(Stream: TStream; Defines: TMemoryStream; StrList: TStringList; ObjList: TStringList; AddObjList: TStringList; var AIdx: Integer);
var
  Idx, Idy: Integer;
  bt: Byte;
  lItem: TCustomParam;
begin

  FParamName.WriteToStream(Stream);
  Stream.Write(bByte, 1);
  Stream.Write(wColCount, 2);
  Stream.Write(wSize, 2);
  Stream.Write(wnSize, 2);
  Stream.Write(bNewFld, 1);

  if bNewFld = 0 then
  begin
    for Idx := 0 to wColCount - 1 do
    begin
      bt := BYTE(FFieldsList[Idx]);
      Stream.Write(bt, 1);
    end;
  end;

  for Idx := 0 to wColCount - 1 do
  begin
  lItem := TCustomParam(FItemsList[Idx]);
  if (Byte(FFieldsList[Idx]) in [4, 11]) and (ObjList.Count > 0) then
    begin
      //lItem.ParamData[0] := DWORD(ObjList.Objects[lItem.ParamData[0]]);
      for Idy := 0 to lItem.Count - 1 do
        if TParamItem(DWORD(lItem.ParamData[Idy])).FIsImported then
        begin
          if AddObjList.IndexOf(TParamItem(DWORD(lItem.ParamData[Idy])).FParamName.ParamData[0]) = -1 then
          begin
            TParamItem(DWORD(lItem.ParamData[Idy])).FIndex := AIdx;
            Inc(AIdx);
            AddObjList.AddObject(TParamItem(DWORD(lItem.ParamData[Idy])).FParamName.ParamData[0], TParamItem(DWORD(lItem.ParamData[Idy])))
          end;
        end;

    end;
    lItem.WriteToStream (Stream);
  end;



end;


{ TCustomParam }

procedure TCustomParam.Clear;
begin
///
end;

function TCustomParam.Count: Integer;
begin
  Result := 1;
end;

function TCustomParam.GetParamData(Index: Integer): Variant;
begin
// do nothing
end;

function TCustomParam.IsAddrSored: Boolean;
begin
  Result := False;
end;

procedure TCustomParam.ReadFromStream(Stream: TStream);
begin
// do nothing
end;


procedure TCustomParam.SetParamData(Index: Integer; const Value: Variant);
begin
// do nothing
end;

procedure TCustomParam.WriteToStream(Stream: TStream);
begin
// do nothing
end;


{ TIntegerParam }

constructor TIntegerParam.Create(AddrStore: Boolean);
begin
  FAddrStore := AddrStore;
end;

function TIntegerParam.GetParamData(Index: Integer): Variant;
begin
  Result := FParamData;
end;

function TIntegerParam.IsAddrSored: Boolean;
begin
  Result := FAddrStore;
end;

procedure TIntegerParam.ReadFromStream(Stream: TStream);
begin
  inherited;
  Stream.Read(FParamData, 4);
end;

procedure TIntegerParam.SetParamData(Index: Integer; const Value: Variant);
begin
  inherited;
  FParamData :=  Value;
end;

procedure TIntegerParam.WriteToStream(Stream: TStream);
var
  wr: DWORD;
begin
  inherited;
  wr := FParamData;
   if FAddrStore then
     wr := TParamItem(wr).FIndex;
  Stream.Write(wr, 4);
end;


{ TFloatParam }

function TFloatParam.GetParamData(Index: Integer): Variant;
begin
  Result := FParamData;
end;

procedure TFloatParam.ReadFromStream(Stream: TStream);
begin
  inherited;
  Stream.Read(FParamData, 4);
end;

procedure TFloatParam.SetParamData(Index: Integer; const Value: Variant);
begin
  inherited;
  FParamData :=  Value;
end;

procedure TFloatParam.WriteToStream(Stream: TStream);
begin
  inherited;
  Stream.Write(FParamData, 4);
end;

{ TWordParam }

function TWordParam.GetParamData(Index: Integer): Variant;
begin
  Result := FParamData;
end;

procedure TWordParam.ReadFromStream(Stream: TStream);
begin
  inherited;
  Stream.Read(FParamData, 2);
end;

procedure TWordParam.SetParamData(Index: Integer; const Value: Variant);
begin
  inherited;
  FParamData :=  Value;
end;

procedure TWordParam.WriteToStream(Stream: TStream);
begin
  inherited;
  Stream.Write(FParamData, 2);
end;

{ TByteParam }

function TByteParam.GetParamData(Index: Integer): Variant;
begin
  Result := FParamData;
end;

procedure TByteParam.ReadFromStream(Stream: TStream);
begin
  inherited;
  Stream.Read(FParamData, 1);
end;

procedure TByteParam.SetParamData(Index: Integer; const Value: Variant);
begin
  inherited;
  FParamData :=  Value;
end;

procedure TByteParam.WriteToStream(Stream: TStream);
begin
  inherited;
  Stream.Write(FParamData, 1);
end;

{ TStringParam }

constructor TStringParam.Create(StringStream: TMemoryStream; StringList: TStringList);
begin
  FStringStream := StringStream;
  FStringList := StringList;
end;

function TStringParam.GetParamData(Index: Integer): Variant;
begin
   Result := FParamData;
end;

procedure TStringParam.ReadFromStream(Stream: TStream);
var
  Cnt: Integer;
begin
  inherited;
  if FStringStream = nil then
  begin
    Stream.Read(Cnt, 4);
    SetLength(FParamData, Cnt);
    Stream.Read(FParamData[1], Cnt);
    Exit;
  end;
  Stream.Read(Cnt, 4);
  if FStringStream.Size < Cnt then
    Exit;
  FParamData := AnsiString(GetMString(Cnt, FStringStream));
end;

procedure TStringParam.SetParamData(Index: Integer; const Value: Variant);
begin
  inherited;
  FParamData :=  AnsiString(Value);
end;

function TStringParam.StringList: TStringList;
begin
  Result := FStringList;
end;

function TStringParam.StringStream: TMemoryStream;
begin
  Result := FStringStream;
end;

procedure TStringParam.WriteToStream(Stream: TStream);
var
  Cnt: Integer;
  byt: Byte;
begin
  inherited;
  if (FStringStream = nil) then
  begin
    Cnt := Length(FParamData);
    Stream.Write(Cnt, 4);
    Stream.Write(FParamData[1], Cnt);
    Exit;
  end;

  Cnt := FStringList.IndexOf(String(FParamData));

  if FParamData = '' then
    Cnt := -1
  else
  begin
    if Cnt <> -1 then
    begin
      Cnt := Integer(FStringList.Objects[Cnt]);
    end
    else
    begin
      byt := 0;
      if FStringStream.Position = 0 then
        FStringStream.Write(byt, 1);
      Cnt := FStringStream.Position;
      FStringStream.Write(FParamData[1], Length(FParamData));
      FStringStream.Write(byt, 1);
      FStringList.AddObject(String(FParamData), TObject(Cnt));
    end;
  end;

  Stream.Write(Cnt, 4);
end;

{ TWordParamSet }

procedure TWordParamSet.Clear;
begin
  inherited;
  FParamDataList.Clear;
end;

function TWordParamSet.Count: Integer;
begin
  Result := FParamDataList.Count;
end;

constructor TWordParamSet.Create;
begin
  FParamDataList := TList.Create;
end;

destructor TWordParamSet.Destroy;
begin
  FParamDataList.Free;
  inherited;
end;

function TWordParamSet.GetParamData(Index: Integer): Variant;
begin
  if (Index >= 0) and (Index < FParamDataList.Count) then
    Result := Word(FParamDataList[Index])
  else
    Result := null;
end;

procedure TWordParamSet.ReadFromStream(Stream: TStream);
var
  bExist: BYTE;
  Cnt, Idx: Integer;
  wr: WORD;
begin
  inherited;
  Stream.Read(bExist, 1);
  if bExist = 1 then
  begin
    Stream.Read(Cnt, 4);
    for Idx := 1 to Cnt do
    begin
      Stream.Read(wr, 2);
      FParamDataList.Add(TObject(wr));
    end;
  end;
end;

procedure TWordParamSet.SetParamData(Index: Integer; const Value: Variant);
begin
  inherited;
  if Index = -1  then
    FParamDataList.Add(TObject(Word(Value)))
  else if (Index >= 0) and (Index < FParamDataList.Count) then
    FParamDataList[Index] := TObject(Word(Value));
end;

procedure TWordParamSet.WriteToStream(Stream: TStream);
var
  bExist: BYTE;
  Idx: Integer;
  wr: WORD;
begin
  inherited;
  if FParamDataList.Count > 0 then bExist := 1
  else bExist := 0;

  Stream.Write(bExist, 1);
  if bExist = 1 then
  begin
    Stream.Write(FParamDataList.Count, 4);
    for Idx := 0 to FParamDataList.Count - 1 do
    begin
      wr := Word(FParamDataList[Idx]);
      Stream.Write(wr, 2);
    end;
  end;
end;

{ TByteParamSet }

procedure TByteParamSet.Clear;
begin
  inherited;
  FParamDataList.Clear;
end;

function TByteParamSet.Count: Integer;
begin
  Result := FParamDataList.Count;
end;

constructor TByteParamSet.Create;
begin
  FParamDataList := TList.Create;
end;

destructor TByteParamSet.Destroy;
begin
  FParamDataList.Free;
  inherited;
end;

function TByteParamSet.GetParamData(Index: Integer): Variant;
begin
  if (Index >= 0) and (Index < FParamDataList.Count) then
    Result := Byte(FParamDataList[Index])
  else
    Result := null;
end;

procedure TByteParamSet.ReadFromStream(Stream: TStream);
var
  bExist: BYTE;
  Cnt, Idx: Integer;
  wr: Byte;
begin
  inherited;
  Stream.Read(bExist, 1);
  if bExist = 1 then
  begin
    Stream.Read(Cnt, 4);
    for Idx := 1 to Cnt do
    begin
      Stream.Read(wr, 1);
      FParamDataList.Add(TObject(wr));
    end;
  end;
end;

procedure TByteParamSet.SetParamData(Index: Integer; const Value: Variant);
begin
  inherited;
  if Index = -1  then
    FParamDataList.Add(TObject(Byte(Value)))
  else if (Index >= 0) and (Index < FParamDataList.Count) then
    FParamDataList[Index] := TObject(Byte(Value));
end;

procedure TByteParamSet.WriteToStream(Stream: TStream);
var
  bExist: BYTE;
  Idx: Integer;
  wr: BYTE;
begin
  inherited;
  if FParamDataList.Count > 0 then bExist := 1
  else bExist := 0;

  Stream.Write(bExist, 1);
  if bExist = 1 then
  begin
    Stream.Write(FParamDataList.Count, 4);
    for Idx := 0 to FParamDataList.Count - 1 do
    begin
      wr := BYTE(FParamDataList[Idx]);
      Stream.Write(wr, 1);
    end;
  end;
end;

{ TFloatParamSet }

procedure TFloatParamSet.Clear;
begin
  inherited;
  FParamDataList.Clear;
end;

function TFloatParamSet.Count: Integer;
begin
    Result := FParamDataList.Count;
end;

constructor TFloatParamSet.Create;
begin
  FParamDataList := TList.Create;
end;

destructor TFloatParamSet.Destroy;
begin
  FParamDataList.Free;
  inherited;
end;

function TFloatParamSet.GetParamData(Index: Integer): Variant;
begin
  if (Index >= 0) and (Index < FParamDataList.Count) then
    Result := Single(FParamDataList[Index])
  else
    Result := null;
end;

procedure TFloatParamSet.ReadFromStream(Stream: TStream);
var
  bExist: BYTE;
  Cnt, Idx: Integer;
  wr: Single;
begin
  inherited;
  Stream.Read(bExist, 1);
  if bExist = 1 then
  begin
    Stream.Read(Cnt, 4);
    for Idx := 1 to Cnt do
    begin
      Stream.Read(wr, 4);
      FParamDataList.Add(TObject(wr));
    end;
  end;
end;

procedure TFloatParamSet.SetParamData(Index: Integer; const Value: Variant);
begin
  inherited;
  if Index = -1  then
    FParamDataList.Add(TObject(Single(Value)))
  else if (Index >= 0) and (Index < FParamDataList.Count) then
    FParamDataList[Index] := TObject(Single(Value));
end;

procedure TFloatParamSet.WriteToStream(Stream: TStream);
var
  bExist: BYTE;
  Idx: Integer;
  wr: Single;
begin
  inherited;
  if FParamDataList.Count > 0 then bExist := 1
  else bExist := 0;

  Stream.Write(bExist, 1);
  if bExist = 1 then
  begin
    Stream.Write(FParamDataList.Count, 4);
    for Idx := 0 to FParamDataList.Count - 1 do
    begin
      wr := Single(FParamDataList[Idx]);
      Stream.Write(wr, 4);
    end;
  end;
end;

{ TIntegerParamSet }

procedure TIntegerParamSet.Clear;
begin
  inherited;
  FParamDataList.Clear;
end;

function TIntegerParamSet.Count: Integer;
begin
  Result := FParamDataList.Count;
end;

constructor TIntegerParamSet.Create(AddrStore: Boolean = False);
begin
  FParamDataList := TList.Create;
  FAddrStore := AddrStore;
end;

destructor TIntegerParamSet.Destroy;
begin
  FParamDataList.Free;
  inherited;
end;

function TIntegerParamSet.GetParamData(Index: Integer): Variant;
begin
  if (Index >= 0) and (Index < FParamDataList.Count) then
    Result := Integer(FParamDataList[Index])
  else
    Result := null;
end;

function TIntegerParamSet.IsAddrSored: Boolean;
begin
  Result := FAddrStore;
end;

procedure TIntegerParamSet.ReadFromStream(Stream: TStream);
var
  bExist: BYTE;
  Cnt, Idx: Integer;
  wr: Integer;
begin
  inherited;
  Stream.Read(bExist, 1);
  if bExist = 1 then
  begin
    Stream.Read(Cnt, 4);
    for Idx := 1 to Cnt do
    begin
      Stream.Read(wr, 4);
      FParamDataList.Add(TObject(wr));
    end;
  end;
end;

procedure TIntegerParamSet.SetParamData(Index: Integer; const Value: Variant);
begin
  inherited;
  if Index = -1  then
    FParamDataList.Add(TObject(Integer(Value)))
  else if (Index >= 0) and (Index < FParamDataList.Count) then
    FParamDataList[Index] := TObject(Integer(Value));
end;

procedure TIntegerParamSet.WriteToStream(Stream: TStream);
var
  bExist: BYTE;
  Idx: Integer;
  wr: DWORD;
begin
  inherited;
  if FParamDataList.Count > 0 then bExist := 1
  else bExist := 0;

  Stream.Write(bExist, 1);
  if bExist = 1 then
  begin
    Stream.Write(FParamDataList.Count, 4);
    for Idx := 0 to FParamDataList.Count - 1 do
    begin
      wr := DWORD(FParamDataList[Idx]);
      if FAddrStore then
        wr := TParamItem(wr).FIndex;
      Stream.Write(wr, 4);
    end;
  end;
end;
{ TStringParamSet }

procedure TStringParamSet.Clear;
var
  Idx: Integer;
begin
  inherited;
  for Idx := FParamDataList.Count - 1 downto 0 do
    TObject(FParamDataList[Idx]).Free;
  FParamDataList.Clear;
end;

function TStringParamSet.Count: Integer;
begin
  Result := FParamDataList.Count;
end;

constructor TStringParamSet.Create;
begin
  FParamDataList := TList.Create;
end;

destructor TStringParamSet.Destroy;
var
  Idx: Integer;
begin
  for Idx := FParamDataList.Count - 1 downto 0 do
    TObject(FParamDataList[Idx]).Free;
  FParamDataList.Free;
  inherited;
end;

function TStringParamSet.GetParamData(Index: Integer): Variant;
begin
  if (Index >= 0) and (Index < FParamDataList.Count) then
    Result := TStringParam(FParamDataList[Index]).ParamData[0]
  else
    Result := '';
end;

procedure TStringParamSet.ReadFromStream(Stream: TStream);
var
  bExist: BYTE;
  Cnt, Idx: Integer;
  par: TStringParam;
begin
  inherited;
  Stream.Read(bExist, 1);
  if bExist = 1 then
  begin
    Stream.Read(Cnt, 4);
    for Idx := 1 to Cnt do
    begin
      par := TStringParam.Create(nil, nil);
      par.ReadFromStream(Stream);
      FParamDataList.Add(par);
    end;
  end;
end;

procedure TStringParamSet.SetParamData(Index: Integer; const Value: Variant);
var
  par: TStringParam;
begin
  inherited;
  if Index = -1  then
  begin
    par := TStringParam.Create(nil, nil);
    par.ParamData[0] := Value;
    FParamDataList.Add(par)
  end
  else if (Index >= 0) and (Index < FParamDataList.Count) then
      TStringParam(FParamDataList[Index]).ParamData[0] := Value;
end;

procedure TStringParamSet.WriteToStream(Stream: TStream);
var
  bExist: BYTE;
  Idx: Integer;
  par: TStringParam;
begin
  inherited;
  if FParamDataList.Count > 0 then bExist := 1
  else bExist := 0;
  Stream.Write(bExist, 1);
  if bExist = 1 then
  begin
    Stream.Write(FParamDataList.Count, 4);
    for Idx := 0 to FParamDataList.Count - 1 do
    begin
      par := TStringParam(FParamDataList[Idx]);
      par.WriteToStream(Stream);
    end;
  end;
end;

end.
