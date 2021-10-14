program TW2ParamReader;

uses
  Forms, windows, dialogs, StrUtils, sysutils,
  MainForm in 'MainForm.pas' {Form1},
  ConstantEditor in 'ConstantEditor.pas' {Form2},
  PrefForm in 'PrefForm.pas' {Form3},
  BatchEditF in 'BatchEditF.pas' {BatchEditForm};

{$R *.res}

var
  Params: String;
  ipos, posx, strlen: Integer;
  fFound: Boolean;
  function findparam(pstr: String; idx: Integer): Integer;
  var
    slen:  Integer;
  begin
    slen := Length(pstr);
    while idx <=  slen do
    begin
      if Params[idx] <> ' ' then
      begin
        if Params[idx] = '"' then
        begin
          Result := idx;
          Exit;
        end;
        Result := 0;
        Exit;
      end;
      Inc(idx);
    end;
    Result := 0;
  end;

begin
  //ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TBatchEditForm, BatchEditForm);
  Params := GetCommandLine();
  ipos := PosEx('join', Params, 1);
  strlen := Length(Params);
  fFound := False;
  if (ipos <> 0) and (ipos + 4 < strlen) and (Params[ipos + 4] = ' ') then
  begin
    ipos := ipos + 5;
    fFound := False;
    ipos := findparam(Params, ipos);
    if ipos <> 0 then
    begin
      posx := posex('"', Params, ipos + 1) - 1;
      Form1.FFileNameA := copy(Params, ipos + 1, posx - ipos);
      fFound := True;
      if not FileExists(Form1.FFileNameA) then
        fFound := False;
      ipos := posx + 2;
    end;

    if fFound and (ipos < strlen) and (Params[ipos] = ' ') then
    begin
      Inc(ipos);
      fFound := False;
      ipos := findparam(Params, ipos);
      if ipos <> 0 then
      begin
        posx := posex('"', Params, ipos + 1) - 1;
        Form1.FFileNameB := copy(Params, ipos + 1, posx - ipos);
        fFound := True;
        if not FileExists(Form1.FFileNameB) then
          fFound := False;
        ipos := posx + 2;
      end;
    end;

    if fFound and (ipos < strlen) and (Params[ipos] = ' ') then
    begin
      Inc(ipos);
      fFound := False;
      ipos := findparam(Params, ipos);
      if ipos <> 0 then
      begin
        posx := posex('"', Params, ipos + 1) - 1;
        Form1.FFileNameC := copy(Params, ipos + 1, posx - ipos);
        fFound := True;
        if not FileExists(Form1.FFileNameC) then
          fFound := False;
        ipos := posx + 2;
      end;
    end;
  end;

   if fFound then
   begin
     Form1.TBOpenClick(nil);
     Form1.Joinparfiles1Click(Form1.ToolButton13);
     Application.Terminate;
   end
   else
    Application.Run;
end.
