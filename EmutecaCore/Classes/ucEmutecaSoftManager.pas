unit ucEmutecaSoftManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8,
  uaEmutecaManager, ucEmutecaSoftware;

resourcestring
  rsLoadingVersionList = 'Loading software list...';
  rsSavingVersionList = 'Saving software list...';

type

  { cEmutecaSoftManager }

  cEmutecaSoftManager = class(caEmutecaManagerTxt)
  private
    FVisibleList: cEmutecaSoftList;
    FFullList: cEmutecaSoftList;

  protected


  public
    property FullList: cEmutecaSoftList read FFullList;
    {< Actual list where the software is stored. }
    property VisibleList: cEmutecaSoftList read FVisibleList;
    {< Filtered soft list }

    procedure LoadFromFileTxt(TxtFile: TStrings); override;
    procedure SaveToFileTxt(TxtFile: TStrings; const ExportMode: boolean);
      override;

    function ItemById(aId: string): cEmutecaSoftware;
    {< Returns the version with aId key.

       @Result cEmutecaSoftware found or nil.
    }

    procedure FilterBySystem(aSystemKey: string);

    procedure AssingAllTo(aList: TStrings); override;
    procedure AssingEnabledTo(aList: TStrings); override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
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

procedure cEmutecaSoftManager.FilterBySystem(aSystemKey: string);
var
  i: longint;
  aSoft: cEmutecaSoftware;
begin
  VisibleList.Clear;

  if aSystemKey = '' then
  begin
    VisibleList.Assign(FullList);
  end
  else
  begin
    i := 0;
    while i < FullList.Count do
    begin
      aSoft := cEmutecaSoftware(FullList[i]);
      if UTF8CompareText(aSoft.SystemKey, aSystemKey) = 0 then
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

procedure cEmutecaSoftManager.LoadFromFileTxt(TxtFile: TStrings);
var
  i: integer;
  TempVersion: cEmutecaSoftware;
begin
  if not Assigned(TxtFile) then
    Exit;

  FullList.Capacity := FullList.Count + TxtFile.Count; // Speed Up?
  TxtFile.BeginUpdate;
  i := 1; // Skipping Header
  while i < TxtFile.Count do
  begin
    TempVersion := cEmutecaSoftware.Create(nil);
    TempVersion.DataString := TxtFile[i];
    FullList.Add(TempVersion);
    Inc(i);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsLoadingVersionList, TempVersion.Title,
        TempVersion.Version, i, TxtFile.Count);
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
      ProgressCallBack(rsSavingVersionList, aSoft.SystemKey,
        aSoft.Title, i, FullList.Count);
  end;
  TxtFile.EndUpdate;
end;

end.
