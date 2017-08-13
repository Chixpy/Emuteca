unit ucEmutecaSoftManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazUTF8, LazFileUtils, IniFiles,
  uaEmutecaCustomManager,
  uEmutecaCommon,
  uaEmutecaCustomSystem, uaEmutecaCustomGroup,
  ucEmutecaSoftList;

type

  { cEmutecaSoftManager }

  cEmutecaSoftManager = class(caEmutecaCustomManager)
  private
    FProgressCallBack: TEmutecaProgressCallBack;
    FSystem: caEmutecaCustomSystem;
    FFilterGroup: caEmutecaCustomGroup;
    FVisibleList: cEmutecaSoftList;
    FFullList: cEmutecaSoftList;
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack);
    procedure SetSystem(AValue: caEmutecaCustomSystem);
    procedure SetFilterGroup(AValue: caEmutecaCustomGroup);

  protected

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

    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressCallBack;

    procedure LoadFromStrLst(TxtFile: TStrings); override;
    procedure ImportFromStrLst(aTxtFile: TStrings); override;
    procedure SaveToStrLst(TxtFile: TStrings; const ExportMode: boolean);
      override;
    procedure LoadFromIni(aIniFile: TMemIniFile); override;
    procedure ImportFromIni(aIniFile: TMemIniFile); override;
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

procedure cEmutecaSoftManager.LoadFromIni(aIniFile: TMemIniFile);
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
      ProgressCallBack(rsSavingSystemList, aSoft.Title, aSoft.Version,
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

procedure cEmutecaSoftManager.ImportFromIni(aIniFile: TMemIniFile);
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
      ProgressCallBack(rsImportingSystemList, aSoft.Title, aSoft.Version,
        i, FullList.Count);

      aSoft.LoadFromIni(aIniFile);
      Inc(i);

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', '', 0, 0);
end;

procedure cEmutecaSoftManager.ImportFromStrLst(aTxtFile: TStrings);
begin

end;

procedure cEmutecaSoftManager.SetProgressCallBack(
  AValue: TEmutecaProgressCallBack);
begin
  if FProgressCallBack = AValue then
    Exit;
  FProgressCallBack := AValue;
end;

procedure cEmutecaSoftManager.LoadFromStrLst(TxtFile: TStrings);
var
  i: integer;
  TempSoft: cEmutecaSoftware;
begin
  if not Assigned(TxtFile) then
    Exit;

  // FullList.BeginUpdate;
  FullList.Capacity := FullList.Count + TxtFile.Count; // Speed Up?
  i := 1; // Skipping Header
  while i < TxtFile.Count do
  begin
    TempSoft := cEmutecaSoftware.Create(nil);
    TempSoft.TXTString := TxtFile[i];
    TempSoft.CachedSystem := System;
    FullList.Add(TempSoft);

    if Assigned(ProgressCallBack) then
      ProgressCallBack(rsLoadingSystemList, TempSoft.Title, TempSoft.Version,
        i, TxtFile.Count);

    Inc(i);
  end;
  // FullList.EndUpdate;

  VisibleList.Assign(FullList);
  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', '', 0, 0);

end;

procedure cEmutecaSoftManager.SaveToStrLst(TxtFile: TStrings;
  const ExportMode: boolean);
var
  i: integer;
  aSoft: cEmutecaSoftware;
begin
  if not Assigned(TxtFile) then
    Exit;

  { TODO : cEmutecaSoftManager.SaveToStrLst Export mode }
  TxtFile.Clear;
  TxtFile.BeginUpdate;
  try
    TxtFile.Capacity := FullList.Count + 1; // Speed up?

    if ExportMode then
      TxtFile.Add(krsCSVSoftHeader)
    else
      TxtFile.Add(krsCSVSoftStatsHeader);

    i := 0;
    while i < FullList.Count do
    begin
      aSoft := FullList[i];
      if Assigned(ProgressCallBack) then
        ProgressCallBack(rsLoadingSystemList, aSoft.Title, aSoft.Version,
          i, FullList.Count);
      TxtFile.Add(aSoft.TXTString);
      Inc(i);
    end;

  finally
    TxtFile.EndUpdate;
  end;
  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', '', 0, 0);
end;

end.
