unit ucEmutecaSoftManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazUTF8, LazFileUtils, IniFiles,
  uEmutecaCommon,
  uaEmutecaCustomManager,
  uaEmutecaCustomSystem, uaEmutecaCustomGroup,
  ucEmutecaSoftList;

type

  { cEmutecaSoftManager }

  cEmutecaSoftManager = class(caEmutecaCustomManager)
  private
    FSystem: caEmutecaCustomSystem;
    FFilterGroup: caEmutecaCustomGroup;
    FVisibleList: cEmutecaSoftList;
    FFullList: cEmutecaSoftList;
    procedure SetSystem(AValue: caEmutecaCustomSystem);
    procedure SetFilterGroup(AValue: caEmutecaCustomGroup);

  protected
    procedure ActLoadStrLst(aSoftLst: cEmutecaSoftList; aTxtFile: TStrings);

  public
    procedure ClearData;
    //< Clears all data WITHOUT saving.
    procedure LoadData;
    //< Reload last data file WITHOUT saving changes.

    property System: caEmutecaCustomSystem read FSystem write SetSystem;
    property FilterGroup: caEmutecaCustomGroup
      read FFilterGroup write SetFilterGroup;

    property VisibleList: cEmutecaSoftList read FVisibleList;
    {< Filtered soft list }

    procedure LoadFromStrLst(aTxtFile: TStrings); override;
    procedure ImportFromStrLst(aTxtFile: TStrings); override;
    procedure SaveToStrLst(aTxtFile: TStrings; const ExportMode: boolean);
      override;
    procedure LoadFromIni(aIniFile: TIniFile); override;
    procedure ImportFromIni(aIniFile: TIniFile); override;
    procedure SaveToIni(aIniFile: TMemIniFile; const ExportMode: boolean);
      override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property FullList: cEmutecaSoftList read FFullList;
    {< Actual list where the software is stored. }
  end;

implementation

uses uaEmutecaCustomSoft, ucEmutecaSoftware;

{ cEmutecaSoftManager }

constructor cEmutecaSoftManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FFullList := cEmutecaSoftList.Create(True);
  FVisibleList := cEmutecaSoftList.Create(False);
  // TODO: OnCompare FullList.OnCompare := ;
end;

destructor cEmutecaSoftManager.Destroy;
begin
  FVisibleList.Free;
  FFullList.Free;
  inherited Destroy;
end;

procedure cEmutecaSoftManager.LoadFromIni(aIniFile: TIniFile);
begin

end;

procedure cEmutecaSoftManager.SaveToIni(aIniFile: TMemIniFile;
  const ExportMode: boolean);
var
  aSoft: cEmutecaSoftware;
  i: integer;
begin
  if not Assigned(aIniFile) then
    Exit;

  // If not export mode remove file data
  if not ExportMode then
    aIniFile.Clear;

  try
    i := 0;
    while i < FullList.Count do
    begin
      aSoft := FullList[i];

      if Assigned(ProgressCallBack) then
        ProgressCallBack(rsSavingSoftList, aSoft.Title, aSoft.Version,
          i, FullList.Count);

      aSoft.SaveToIni(aIniFile, ExportMode);
      Inc(i);
    end;
  finally
    aIniFile.UpdateFile;
  end;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', '', 0, 0);
end;

procedure cEmutecaSoftManager.ClearData;
begin
  VisibleList.Clear;
  FullList.Clear;
end;

procedure cEmutecaSoftManager.LoadData;
begin
  ClearData;
  LoadFromFileTxt('');
end;

procedure cEmutecaSoftManager.SetFilterGroup(AValue: caEmutecaCustomGroup);
var
  i: integer;
  aSoft: cEmutecaSoftware;
begin
  if FFilterGroup = AValue then
    Exit;
  FFilterGroup := AValue;

  VisibleList.Clear;

  // Filter by FilterGroup
  if Assigned(FilterGroup) then
  begin
    i := 0;
    while (i < FullList.Count) do
    begin
      aSoft := FullList[i];
      if aSoft.CachedGroup = FilterGroup then
        VisibleList.Add(aSoft);
      Inc(i);
    end;
  end
  else
  begin
    VisibleList.Assign(FullList);
  end;
end;

procedure cEmutecaSoftManager.ActLoadStrLst(aSoftLst: cEmutecaSoftList;
  aTxtFile: TStrings);
var
  aSoft: cEmutecaSoftware;
  i: integer;
begin
  // aSoftLst.BeginUpdate;
  aSoftLst.Capacity := aSoftLst.Count + aTxtFile.Count; // Speed Up?
  i := 1; // Skipping Header
  while i < aTxtFile.Count do
  begin
    aSoft := cEmutecaSoftware.Create(nil);
    aSoft.TXTString := aTxtFile[i];
    aSoft.CachedSystem := System;
    aSoftLst.Add(aSoft);

    if Assigned(ProgressCallBack) then
      ProgressCallBack(rsLoadingSoftList, aSoft.Title, aSoft.Version,
        i, aTxtFile.Count);

    Inc(i);
  end;
  // aSoftLst.EndUpdate;
end;

procedure cEmutecaSoftManager.SetSystem(AValue: caEmutecaCustomSystem);
var
  aSoft: cEmutecaSoftware;
  i: integer;

begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;

  i := 0;
  while i < FullList.Count do
  begin
    aSoft := cEmutecaSoftware(FullList[i]);
    aSoft.CachedSystem := System;
  end;
end;

procedure cEmutecaSoftManager.ImportFromIni(aIniFile: TIniFile);
var
  aSoft: cEmutecaSoftware;
  i: integer;
begin
  if not Assigned(aIniFile) then
    Exit;

  i := 0;
  while i < FullList.Count do
  begin
    aSoft := FullList[i];

    if Assigned(ProgressCallBack) then
      ProgressCallBack(rsImportingSoftList, aSoft.Title, aSoft.Version,
        i, FullList.Count);

    aSoft.LoadFromIni(aIniFile);
    Inc(i);
  end;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', '', 0, 0);
end;

procedure cEmutecaSoftManager.ImportFromStrLst(aTxtFile: TStrings);
var
  aSoftLst: cEmutecaSoftList;
  i, j, aComp: integer;
  aSoft1, aSoft2: cEmutecaSoftware;
begin
  if not Assigned(aTxtFile) then
    Exit;

  aSoftLst := cEmutecaSoftList.Create(True);
  try
    // Loading import group list
    ActLoadStrLst(aSoftLst, aTxtFile);

    aSoftLst.Sort(@EmutecaCompareSoftByID);
    FullList.Sort(@EmutecaCompareSoftByID);

    i := aSoftLst.Count - 1;
    if i >= 0 then
      aSoft2 := aSoftLst[i]
    else
      aSoft2 := nil;
    j := FullList.Count;
    while j > 0 do
    begin
      Dec(j);
      aSoft1 := FullList[j];

      if assigned(ProgressCallBack) then
        ProgressCallBack(rsImportingGroupList, aSoft1.Title, aSoft1.ID,
          j, FullList.Count);

      if assigned(aSoft2) then
        aComp := aSoft1.CompareID(aSoft2.ID)
      else
        aComp := 1; // aSoft1.CompareGroupKey('');

      // aSoft1 < aSoft2 -> Try Previous group2
      while aComp < 0 do
      begin
        Dec(i);
        if i >= 0 then
          aSoft2 := aSoftLst[i]
        else
          aSoft2 := nil;

        if assigned(aSoft2) then
          aComp := aSoft1.CompareID(aSoft2.ID)
        else
          aComp := 1; // aSoft1.CompareGroupKey('');
      end;
      // aSoft1 > aSoft2 -> Not found.
      // aSoft1 = aSoft2 -> Match.
      if aComp = 0 then
        aSoft1.ImportFrom(aSoft2);
    end;

  finally
    aSoftLst.Free;
  end;
  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', '', 0, 0);
end;

procedure cEmutecaSoftManager.LoadFromStrLst(aTxtFile: TStrings);
begin
  if not Assigned(aTxtFile) then
    Exit;

  ActLoadStrLst(FullList, aTxtFile);

  VisibleList.Assign(FullList);
end;

procedure cEmutecaSoftManager.SaveToStrLst(aTxtFile: TStrings;
  const ExportMode: boolean);

  procedure SaveList(aTxtFile: TStrings);
  var
    i: integer;
    aSoft: cEmutecaSoftware;
  begin

    aTxtFile.Clear;
    aTxtFile.BeginUpdate;
    try
      aTxtFile.Capacity := FullList.Count + 1; // Speed up?
      aTxtFile.Add(krsCSVSoftStatsHeader);

      i := 0;
      while i < FullList.Count do
      begin
        aSoft := FullList[i];
        if Assigned(ProgressCallBack) then
          ProgressCallBack(rsSavingSystemList, aSoft.Title, aSoft.Version,
            i, FullList.Count);
        aTxtFile.Add(aSoft.TXTString);
        Inc(i);
      end;
    finally
      aTxtFile.EndUpdate;
    end;
    if assigned(ProgressCallBack) then
      ProgressCallBack('', '', '', 0, 0);

  end;

  procedure ExportList(aTxtFile: TStrings);
  var
    i: integer;
    aSoft: cEmutecaSoftware;
  begin

    { TODO: Read items in file, merge and save. }

    aTxtFile.Clear;
    aTxtFile.BeginUpdate;
    try
      aTxtFile.Capacity := FullList.Count + 1; // Speed up?
      aTxtFile.Add(krsCSVSoftHeader);

      i := 0;
      while i < FullList.Count do
      begin
        aSoft := FullList[i];
        if Assigned(ProgressCallBack) then
          ProgressCallBack(rsSavingSystemList, aSoft.Title, aSoft.Version,
            i, FullList.Count);

        aTxtFile.Add(aSoft.TXTExportString);

        Inc(i);
      end;
    finally
      aTxtFile.EndUpdate;
    end;
    if assigned(ProgressCallBack) then
      ProgressCallBack('', '', '', 0, 0);
  end;

begin
  if not Assigned(aTxtFile) then
    Exit;

  if ExportMode then
    ExportList(aTxtFile)
  else
    SaveList(aTxtFile);
end;

end.
