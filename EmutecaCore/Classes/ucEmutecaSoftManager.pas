unit ucEmutecaSoftManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazUTF8, LazFileUtils, IniFiles,
  uCHXStrUtils, uaCHXStorable,
  uEmutecaCommon, ucEmutecaSystemManager,
  ucEmutecaConfig, ucEmutecaSystem, ucEmutecaGroup, ucEmutecaSoftware;

resourcestring
  rsLoadingVersionList = 'Loading software list...';
  rsSavingVersionList = 'Saving software list...';

type

  { cEmutecaSoftManager }

  cEmutecaSoftManager = class(caCHXStorableTxt)
  private
    FConfig: cEmutecaConfig;
    FProgressCallBack: TEmutecaProgressCallBack;
    FVisibleGroup: cEmutecaGroup;
    FSysDataFolder: string;
    FSysManager: cEmutecaSystemManager;
    FVisibleSystem: cEmutecaSystem;
    FVisibleList: cEmutecaSoftList;
    FFullList: cEmutecaSoftList;
    procedure SetConfig(AValue: cEmutecaConfig);
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack);
    procedure SetVisibleGroup(AValue: cEmutecaGroup);
    procedure SetSysDataFolder(AValue: string);
    procedure SetSysManager(AValue: cEmutecaSystemManager);
    procedure SetVisibleSystem(AValue: cEmutecaSystem);

  protected
    procedure UpdateVisibleListByVisibleSystem;

  public
        property Config: cEmutecaConfig read FConfig write SetConfig;

            procedure ClearData;
    //< Clears all data WITHOUT saving.
    procedure ReloadData;
    //< Reload last data file WITHOUT saving changes.

      property ProgressCallBack: TEmutecaProgressCallBack read FProgressCallBack write SetProgressCallBack;
    //< CallBack function to show the progress in actions.

    property VisibleSystem: cEmutecaSystem
      read FVisibleSystem write SetVisibleSystem;
    property VisibleGroup: cEmutecaGroup read FVisibleGroup
      write SetVisibleGroup;

    property VisibleList: cEmutecaSoftList read FVisibleList;
    {< Filtered soft list }

    property SysDataFolder: string read FSysDataFolder write SetSysDataFolder;
    property SysManager: cEmutecaSystemManager
      read FSysManager write SetSysManager;

    procedure AssingAllTo(aList: TStrings);
    procedure AssingEnabledTo(aList: TStrings);

    procedure LoadSoftFromSystems;
    procedure SaveSoftOfSystems(ExportMode: boolean);
    procedure LoadSoftFromSystem(aSystem: cEmutecaSystem);
    procedure SaveSoftOfSystem(aSystem: cEmutecaSystem; ExportMode: boolean);

    procedure LoadFromStrLst(TxtFile: TStrings); override;
    procedure SaveToStrLst(TxtFile: TStrings; const ExportMode: boolean);
      override;
    procedure LoadFromIni(aIniFile: TCustomIniFile); override;
    procedure SaveToIni(IniFile: TCustomIniFile; const ExportMode: boolean);
      override;

    function ItemById(aId: string;
      Autocreate: boolean = False): cEmutecaSoftware;
    {< Returns the software with aId key.

       @Result cEmutecaSoftware found or nil.
    }

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property FullList: cEmutecaSoftList read FFullList;
    {< Actual list where the software is stored. }
  end;

implementation

{ cEmutecaSoftManager }

function cEmutecaSoftManager.ItemById(aId: string;
  Autocreate: boolean): cEmutecaSoftware;
var
  i: integer;
  aSoft: cEmutecaSoftware;
begin
  Result := nil;

  i := 0;
  while (Result = nil) and (i < FullList.Count) do
  begin
    aSoft := FullList[i];
    if UTF8CompareText(aSoft.ID, aId) = 0 then
      Result := aSoft;
    Inc(i);
  end;
end;

constructor cEmutecaSoftManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FFullList := cEmutecaSoftList.Create(True);
  FVisibleList := cEmutecaSoftList.Create(False);
  // TODO: OnCompare FullList.OnCompare := ;
end;

destructor cEmutecaSoftManager.Destroy;
begin
  FreeAndNil(FVisibleList);
  FreeAndNil(FFullList);
  inherited Destroy;
end;

procedure cEmutecaSoftManager.LoadFromIni(aIniFile: TCustomIniFile);
begin

end;

procedure cEmutecaSoftManager.SaveToIni(IniFile: TCustomIniFile;
  const ExportMode: boolean);
var
  aSoft: cEmutecaSoftware;
  i: integer;
begin
  if not Assigned(IniFile) then
    Exit;

  try
    i := 0;
    while i < FullList.Count do
    begin
      aSoft := FullList[i];
      aSoft.SaveToIni(IniFile, ExportMode);
      Inc(i);

      if Assigned(ProgressCallBack) then
        ProgressCallBack(rsSavingVersionList, aSoft.Folder,
          aSoft.FileName, i, FullList.Count);
    end;
  finally
    IniFile.UpdateFile;
  end;
end;

procedure cEmutecaSoftManager.SetSysManager(AValue: cEmutecaSystemManager);
begin
  if FSysManager = AValue then
    Exit;
  FSysManager := AValue;
end;

procedure cEmutecaSoftManager.SetVisibleSystem(AValue: cEmutecaSystem);
var
  i: integer;
  aSoft: cEmutecaSoftware;
begin
  if FVisibleSystem = AValue then
    Exit;
  FVisibleSystem := AValue;

  FVisibleGroup := nil; // We don't want trigger SetVisibleGroup

  UpdateVisibleListByVisibleSystem;
end;

procedure cEmutecaSoftManager.UpdateVisibleListByVisibleSystem;
var
  i: integer;
  aSoft: cEmutecaSoftware;
begin
  // Common and repeated code...

  VisibleList.Clear;
  if Assigned(VisibleSystem) then
  begin
    // Filter by VisibleSystem
    i := 0;
    while (i < FullList.Count) do
    begin
      aSoft := FullList[i];
      if aSoft.System = VisibleSystem then
        VisibleList.Add(aSoft);
      Inc(i);
    end;
  end
  else
  begin
    VisibleList.Assign(FullList);
  end;
end;

procedure cEmutecaSoftManager.ClearData;
begin
  VisibleList.Clear;
  FullList.Clear;
end;

procedure cEmutecaSoftManager.ReloadData;
begin
  ClearData;

  LoadSoftFromSystems;
end;

procedure cEmutecaSoftManager.SetSysDataFolder(AValue: string);
begin
  FSysDataFolder := SetAsFolder(AValue);
end;

procedure cEmutecaSoftManager.SetVisibleGroup(AValue: cEmutecaGroup);
var
  i: integer;
  aSoft: cEmutecaSoftware;
begin
  if FVisibleGroup = AValue then
    Exit;
  FVisibleGroup := AValue;

  // Filter by VisibleGroup
  if Assigned(VisibleGroup) then
  begin
    VisibleList.Clear;
    // We don't want trigger SetVisibleGroup
    FVisibleSystem := cEmutecaSystem(VisibleGroup.System);
    i := 0;
    while (i < FullList.Count) do
    begin
      aSoft := FullList[i];
      if aSoft.Group = VisibleGroup then
        VisibleList.Add(aSoft);
      Inc(i);
    end;
  end
  else
  begin
    // Keep current VisibleSystem
    //   FVisibleSystem := nil;

    UpdateVisibleListByVisibleSystem;
  end;
end;

procedure cEmutecaSoftManager.SetConfig(AValue: cEmutecaConfig);
begin
  if FConfig = AValue then Exit;
  FConfig := AValue;
end;

procedure cEmutecaSoftManager.SetProgressCallBack(
  AValue: TEmutecaProgressCallBack);
begin
  if FProgressCallBack = AValue then Exit;
  FProgressCallBack := AValue;
end;

procedure cEmutecaSoftManager.AssingAllTo(aList: TStrings);
begin

end;

procedure cEmutecaSoftManager.AssingEnabledTo(aList: TStrings);
begin

end;

procedure cEmutecaSoftManager.LoadSoftFromSystems;
var
  i: integer;
  aSys: cEmutecaSystem;
begin
  if not assigned(SysManager) then
    Exit;

  i := 0;
  while i < SysManager.EnabledList.Count do
  begin
    aSys := SysManager.EnabledList[i];

    if ProgressCallBack <> nil then
      ProgressCallBack(rsLoadingVersionList, aSys.Title,
        '', i, SysManager.EnabledList.Count);

    LoadSoftFromSystem(aSys);

    Inc(i);
  end;

  if ProgressCallBack <> nil then
    ProgressCallBack(rsLoadingVersionList, '',
      '', 0, 0);
end;

procedure cEmutecaSoftManager.SaveSoftOfSystems(ExportMode: boolean);
var
  i: integer;
  aSys: cEmutecaSystem;
begin
  if not assigned(SysManager) then
    Exit;

  i := 0;
  while i < SysManager.EnabledList.Count do
  begin
    aSys := SysManager.EnabledList[i];

    if ProgressCallBack <> nil then
      ProgressCallBack(rsSavingVersionList, aSys.Title,
        '', i, SysManager.EnabledList.Count);

    SaveSoftOfSystem(aSys, ExportMode);

    Inc(i);
  end;

  if ProgressCallBack <> nil then
    ProgressCallBack(rsSavingVersionList, '',
      '', 0, 0);
end;

procedure cEmutecaSoftManager.LoadSoftFromSystem(aSystem: cEmutecaSystem);
var
  i: integer;
  aFile: string;
  aSoft: cEmutecaSoftware;
  TxtFile: TStringList;
begin
  if not assigned(aSystem) then
    Exit;
  aFile := SysDataFolder + aSystem.FileName + kEmutecaSoftFileExt;
  if not FileExistsUTF8(aFile) then
    Exit;

  TxtFile := TStringList.Create;
  try
    TxtFile.LoadFromFile(aFile);

    // FullList.BeginUpdate;
    FullList.Capacity := FullList.Count + TxtFile.Count; // Speed Up?
    i := 1; // Skipping Header
    while i < TxtFile.Count do
    begin
      aSoft := cEmutecaSoftware.Create(nil);
      aSoft.TXTString := TxtFile[i];
      aSoft.System := aSystem;

      FullList.Add(aSoft);
      Inc(i);
    end;
    // FullList.EndUpdate;
  finally
    TxtFile.Free;
  end;
end;

procedure cEmutecaSoftManager.SaveSoftOfSystem(aSystem: cEmutecaSystem;
  ExportMode: boolean);
var
  i: integer;
  aFile: string;
  aSoft: cEmutecaSoftware;
  TxtFile: TStringList;
begin
  if not assigned(aSystem) then
    Exit;
  aFile := SysDataFolder + aSystem.FileName + kEmutecaSoftFileExt;

  TxtFile := TStringList.Create;
  TxtFile.BeginUpdate;
  try
    TxtFile.Capacity := FullList.Count + 1; // Speed up?
    if ExportMode then
      TxtFile.Add(krsCSVSoftHeader)
    else
      TxtFile.Add(krsCSVSoftStatsHeader);

    // TODO: Necesito que el sistema tenga una lista de juegos para hacerlo
    //   más rápido ¿TObjectList?
    i := 0;
    while i < FullList.Count do
    begin
      aSoft := FullList[i];
      if aSoft.System = aSystem then
      begin
        if not ExportMode then
          TxtFile.Add(aSoft.TXTString);
      end;
      Inc(i);
    end;

  finally
    TxtFile.EndUpdate;
    TxtFile.SaveToFile(aFile);
    TxtFile.Free;
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

    FullList.Add(TempSoft);
    Inc(i);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsLoadingVersionList, TempSoft.Folder,
        TempSoft.FileName, i, TxtFile.Count);
  end;
  // FullList.EndUpdate;

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

      if ProgressCallBack <> nil then
        ProgressCallBack(rsSavingVersionList, aSoft.Folder,
          aSoft.FileName, i, FullList.Count);
    end;

  finally
    TxtFile.EndUpdate;
  end;
end;

end.
