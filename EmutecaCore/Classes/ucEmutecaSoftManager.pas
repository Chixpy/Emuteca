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
    FEnabledList: cEmutecaSoftList;
    FFullList: cEmutecaSoftList;

  protected


  public
    property FullList: cEmutecaSoftList read FFullList;
    {< Actual list where the software is stored. }
    property EnabledList: cEmutecaSoftList read FEnabledList;
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
  EnabledList.Clear;

  if aSystemKey = '' then
  begin
    EnabledList.Assign(FullList);
  end
  else
  begin
    i := 0;
    while i < FullList.Count do
    begin
      aSoft := cEmutecaSoftware(FullList[i]);
      if UTF8CompareText(aSoft.System, aSystemKey) = 0 then
        EnabledList.Add(aSoft);
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
  i := 0;
  while i < EnabledList.Count do
  begin
    aSoft := cEmutecaSoftware(EnabledList[i]);
    aList.AddObject(aSoft.Title, aSoft);
    Inc(i);
  end;
  aList.EndUpdate;
end;

constructor cEmutecaSoftManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FFullList := cEmutecaSoftList.Create(True);
  FEnabledList := cEmutecaSoftList.Create(False);
  // TODO: OnCompare FullList.OnCompare := ;
end;

destructor cEmutecaSoftManager.Destroy;
begin
  FreeAndNil(FEnabledList);
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

  EnabledList.Assign(FullList);
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
      ProgressCallBack(rsSavingVersionList, aSoft.System,
        aSoft.Title, i, FullList.Count);
  end;
end;

end.
