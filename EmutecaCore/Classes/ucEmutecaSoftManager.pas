unit ucEmutecaSoftManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazUTF8, LazFileUtils, IniFiles,
  uaCHXStorable,
  uaEmutecaCustomSystem, uaEmutecaCustomGroup,
  ucEmutecaSoftList;

type

  { cEmutecaSoftManager }

  cEmutecaSoftManager = class(caCHXStorableTxt)
  private
    FSystem: caEmutecaCustomSystem;
    FVisibleGroup: caEmutecaCustomGroup;
    FVisibleList: cEmutecaSoftList;
    FFullList: cEmutecaSoftList;
    procedure SetSystem(AValue: caEmutecaCustomSystem);
    procedure SetVisibleGroup(AValue: caEmutecaCustomGroup);

  protected

  public
    procedure ClearData;
    //< Clears all data WITHOUT saving.
    procedure LoadData;
    //< Reload last data file WITHOUT saving changes.

    property System: caEmutecaCustomSystem read FSystem write SetSystem;
    property VisibleGroup: caEmutecaCustomGroup read FVisibleGroup
      write SetVisibleGroup;

    property VisibleList: cEmutecaSoftList read FVisibleList;
    {< Filtered soft list }

    procedure LoadFromStrLst(TxtFile: TStrings); override;
    procedure SaveToStrLst(TxtFile: TStrings; const ExportMode: boolean);
      override;
    procedure LoadFromIni(aIniFile: TMemIniFile); override;
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
      aSoft.SaveToIni(aIniFile, ExportMode);
      Inc(i);
    end;
  finally
    aIniFile.UpdateFile;
  end;
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

procedure cEmutecaSoftManager.SetVisibleGroup(AValue: caEmutecaCustomGroup);
var
  i: integer;
  aSoft: cEmutecaSoftware;
begin
  if FVisibleGroup = AValue then
    Exit;
  FVisibleGroup := AValue;

  VisibleList.Clear;

  // Filter by VisibleGroup
  if Assigned(VisibleGroup) then
  begin
    i := 0;
    while (i < FullList.Count) do
    begin
      aSoft := FullList[i];
      if aSoft.CachedGroup = VisibleGroup then
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
  i: Integer;

begin
  if FSystem = AValue then Exit;
  FSystem := AValue;

    i := 0;
  while i < FullList.Count do
  begin
    aSoft := cEmutecaSoftware(FullList[i]);
    aSoft.CachedSystem := System;
  end;
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
    Inc(i);
  end;
  // FullList.EndUpdate;

  VisibleList.Assign(FullList);
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
      TxtFile.Add(aSoft.TXTString);
      Inc(i);
    end;

  finally
    TxtFile.EndUpdate;
  end;
end;

end.
