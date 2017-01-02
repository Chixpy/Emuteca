unit ucEmutecaSoftManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, IniFiles,
  uaEmutecaManager,
  ucEmutecaSoftware, ucEmutecaSystemManager, ucEmutecaSystem,
  ucEmutecaGroupManager;

resourcestring
  rsLoadingVersionList = 'Loading software list...';
  rsSavingVersionList = 'Saving software list...';

type

  { cEmutecaSoftManager }

  cEmutecaSoftManager = class(caEmutecaManager)
  private
    FGroupManager: cEmutecaGroupManager;
    FSystemManager: cEmutecaSystemManager;
    FVisibleList: cEmutecaSoftList;
    FFullList: cEmutecaSoftList;
    procedure SetGroupManager(AValue: cEmutecaGroupManager);
    procedure SetSystemManager(AValue: cEmutecaSystemManager);

  protected

  public
    property GroupManager: cEmutecaGroupManager
      read FGroupManager write SetGroupManager;
    property SystemManager: cEmutecaSystemManager
      read FSystemManager write SetSystemManager;

    property VisibleList: cEmutecaSoftList read FVisibleList;
    {< Filtered soft list }

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

    procedure FilterBySystem(aSystem: cEmutecaSystem);

    procedure AssingAllTo(aList: TStrings); override;
    procedure AssingEnabledTo(aList: TStrings); override;

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
    aSoft := cEmutecaSoftware(FullList[i]);
    if UTF8CompareText(aSoft.ID, aId) = 0 then
      Result := aSoft;
    Inc(i);
  end;
end;

procedure cEmutecaSoftManager.FilterBySystem(aSystem: cEmutecaSystem);
var
  i: longint;
  aSoft: cEmutecaSoftware;
begin
  VisibleList.Clear;

  if not assigned(aSystem) then
  begin
    VisibleList.Assign(FullList);
  end
  else
  begin
    i := 0;
    while i < FullList.Count do
    begin
      aSoft := cEmutecaSoftware(FullList[i]);
      if aSoft.System = aSystem then
      begin
        if VisibleList.Capacity = VisibleList.Count then
          VisibleList.Capacity := VisibleList.Capacity * 2; // Speed up?
        VisibleList.Add(aSoft);
      end;
      Inc(i);
    end;
  end;
end;

procedure cEmutecaSoftManager.AssingAllTo(aList: TStrings);
var
  i: longint;
  aSoft: cEmutecaSoftware;
begin
  if not assigned(aList) then
    aList := TStringList.Create;

  aList.BeginUpdate;
  aList.Capacity := aList.Count + FullList.Count; // Speed up?
  i := 0;
  while i < FullList.Count do
  begin
    aSoft := cEmutecaSoftware(FullList[i]);
    aList.AddObject(aSoft.Title, aSoft);
    Inc(i);
  end;
  aList.EndUpdate;
end;

procedure cEmutecaSoftManager.AssingEnabledTo(aList: TStrings);
var
  i: longint;
  aSoft: cEmutecaSoftware;
begin
  if not assigned(aList) then
    aList := TStringList.Create;

  aList.BeginUpdate;
  aList.Capacity := aList.Count + VisibleList.Count; // Speed up?
  i := 0;
  while i < VisibleList.Count do
  begin
    aSoft := cEmutecaSoftware(VisibleList[i]);
    aList.AddObject(aSoft.Title, aSoft);
    Inc(i);
  end;
  aList.EndUpdate;
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

procedure cEmutecaSoftManager.SetGroupManager(AValue: cEmutecaGroupManager);
begin
  if FGroupManager = AValue then
    Exit;
  FGroupManager := AValue;
end;

procedure cEmutecaSoftManager.SetSystemManager(AValue: cEmutecaSystemManager);
begin
  if FSystemManager = AValue then
    Exit;
  FSystemManager := AValue;
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
      aSoft := cEmutecaSoftware(FullList[i]);
      aSoft.SaveToIni(IniFile, ExportMode);
      Inc(i);

      if ProgressCallBack <> nil then
        ProgressCallBack(rsSavingVersionList, aSoft.System.ID,
          aSoft.Title, i, FullList.Count);
    end;
  finally
    IniFile.UpdateFile;
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
    TempSoft.DataString := TxtFile[i];

    { TODO : Optimice this: Group now have system link }

    TempSoft.System := SystemManager.ItemById(TempSoft.SystemKey, True);
    TempSoft.Group := GroupManager.ItemById(TempSoft.GroupKey, True);

    FullList.Add(TempSoft);
    Inc(i);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsLoadingVersionList, TempSoft.System.Title,
        TempSoft.Title, i, TxtFile.Count);
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
    TxtFile.Add('"ID","Folder","FileName","Title","Parent","System",' +
      '"Reserved 1","TransliteratedName","SortTitle","Reserved 2",' +
      '"Version","Year","Publisher","Zone","Reserved 3",' +
      '"DumpStatus","DumpInfo","Fixed","Trainer","Translation",' +
      '"Pirate","Cracked","Modified","Hack","Reserved 4",' +
      '"Last Time","Times Played","Playing Time"');

    i := 0;
    while i < FullList.Count do
    begin
      aSoft := cEmutecaSoftware(FullList[i]);
      TxtFile.Add(aSoft.DataString);
      Inc(i);

      if ProgressCallBack <> nil then
        ProgressCallBack(rsSavingVersionList, aSoft.System.ID,
          aSoft.Title, i, FullList.Count);
    end;

  finally
    TxtFile.EndUpdate;
  end;
end;

end.
