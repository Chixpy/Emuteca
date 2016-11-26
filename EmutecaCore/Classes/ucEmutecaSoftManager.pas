unit ucEmutecaSoftManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8,
  uaEmutecaManager,
  ucEmutecaSoftware, ucEmutecaSystemManager, ucEmutecaSystem,
  ucEmutecaGroupManager, ucEmutecaGroup;

resourcestring
  rsLoadingVersionList = 'Loading software list...';
  rsSavingVersionList = 'Saving software list...';

type

  { cEmutecaSoftManager }

  cEmutecaSoftManager = class(caEmutecaManagerTxt)
  private
    FGroupManager: cEmutecaGroupManager;
    FSystemManager: cEmutecaSystemManager;
    FVisibleList: cEmutecaSoftList;
    FFullList: cEmutecaSoftList;
    procedure SetGroupManager(AValue: cEmutecaGroupManager);
    procedure SetSystemManager(AValue: cEmutecaSystemManager);

  protected
    procedure SearchGroup(aSoft: cEmutecaSoftware);
    procedure SearchSystem(aSoft: cEmutecaSoftware);

  public
    property GroupManager: cEmutecaGroupManager read FGroupManager write SetGroupManager;
    property SystemManager: cEmutecaSystemManager read FSystemManager write SetSystemManager;

    property VisibleList: cEmutecaSoftList read FVisibleList;
    {< Filtered soft list }

    procedure LoadFromFileTxt(TxtFile: TStrings); override;
    procedure SaveToFileTxt(TxtFile: TStrings; const ExportMode: boolean);
      override;

    function ItemById(aId: string): cEmutecaSoftware;
    {< Returns the version with aId key.

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


function cEmutecaSoftManager.ItemById(aId: string): cEmutecaSoftware;
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
  if FGroupManager = AValue then Exit;
  FGroupManager := AValue;
end;

procedure cEmutecaSoftManager.SetSystemManager(AValue: cEmutecaSystemManager);
begin
  if FSystemManager = AValue then Exit;
  FSystemManager := AValue;
end;

procedure cEmutecaSoftManager.SearchGroup(aSoft: cEmutecaSoftware);
var
  aGroup: cEmutecaGroup;
begin
  if not assigned(GroupManager) then Exit;

  aSoft.Group := GroupManager.ItemById(aSoft.GroupKey);

  if not assigned(aSoft.Group) then
  begin
    aGroup := cEmutecaGroup.Create(nil);
    aGroup.ID := aSoft.GroupKey;
    aGroup.Title := aSoft.GroupKey;
    aSoft.Group := aGroup;
    GroupManager.FullList.Add(aGroup);
  end;
end;

procedure cEmutecaSoftManager.SearchSystem(aSoft: cEmutecaSoftware);
var
  aSystem: cEmutecaSystem;
begin
  if not assigned(SystemManager) then Exit;

  aSoft.System := SystemManager.ItemById(aSoft.SystemKey);

  if not assigned(aSoft.System) then
  begin
    aSystem := cEmutecaSystem.Create(nil);
    aSystem.ID := aSoft.SystemKey;
    aSystem.Title := aSoft.SystemKey;
    aSoft.System := aSystem;
    SystemManager.FullList.Add(aSystem);
  end;
end;

procedure cEmutecaSoftManager.LoadFromFileTxt(TxtFile: TStrings);
var
  i: integer;
  TempSoft: cEmutecaSoftware;
begin
  if not Assigned(TxtFile) then
    Exit;

  FullList.Capacity := FullList.Count + TxtFile.Count; // Speed Up?
  TxtFile.BeginUpdate;
  i := 1; // Skipping Header
  while i < TxtFile.Count do
  begin
    TempSoft := cEmutecaSoftware.Create(nil);
    TempSoft.DataString := TxtFile[i];

    SearchSystem(TempSoft);
    SearchGroup(TempSoft);

    FullList.Add(TempSoft);
    Inc(i);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsLoadingVersionList, TempSoft.Title,
        TempSoft.Version, i, TxtFile.Count);
  end;
  TxtFile.EndUpdate;
  VisibleList.Assign(FullList);
end;

procedure cEmutecaSoftManager.SaveToFileTxt(TxtFile: TStrings;
  const ExportMode: boolean);
var
  i: integer;
  aSoft: cEmutecaSoftware;
begin
  if not Assigned(TxtFile) then
    Exit;

  { TODO : cEmutecaSoftManager.SaveToFileTxt Export mode }
  TxtFile.Clear;
  TxtFile.BeginUpdate;
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
  TxtFile.EndUpdate;
end;

end.
