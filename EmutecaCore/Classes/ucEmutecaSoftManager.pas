unit ucEmutecaSoftManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8,
  uaEmutecaManager, ucEmutecaSoftware;

resourcestring
  rsLoadingVersionList = 'Loading version list...';
  rsSavingVersionList = 'Saving version list...';

type

  { cEmutecaVersionManager }

  cEmutecaVersionManager = class(caEmutecaManagerTxt)
  private
    FEnabledList: cEmutecaVersionList;
    FFullList: cEmutecaVersionList;
    procedure SetEnabledList(AValue: cEmutecaVersionList);
    procedure SetFullList(AValue: cEmutecaVersionList);

  protected


  public
    property FullList: cEmutecaVersionList read FFullList write SetFullList;
    {< Actual list where the software is stored. }
    property EnabledList: cEmutecaVersionList read FEnabledList write SetEnabledList;
    {< Filtered soft list to show. }

    procedure LoadFromFileTxt(TxtFile: TStrings); override;
    procedure SaveToFileTxt(TxtFile: TStrings; const ExportMode: boolean);
      override;
    function ItemById(aId: string): cEmutecaVersion;
    {< Returns the version with aId key.

       @Result cEmutecaParent found or nil.
    }

    procedure SelectSystem(aSystemKey: String);

    procedure AssingAllTo(aList: TStrings); override;
    procedure AssingEnabledTo(aList: TStrings); override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ cEmutecaVersionManager }


function cEmutecaVersionManager.ItemById(aId: string): cEmutecaVersion;
var
  i: integer;
begin
  Result := nil;

  i := 0;
  while (Result = nil) and (i < FullList.Count) do
  begin
    if UTF8CompareText(FullList[i].ID, aId) = 0 then
      Result := FullList[i];
    Inc(i);
  end;
end;

procedure cEmutecaVersionManager.SelectSystem(aSystemKey: String);
var
  i: longint;
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

    if UTF8CompareText(FullList[i].System, aSystemKey) = 0 then
       EnabledList.Add(FullList[i]);
    Inc(i);
  end;
  end;
end;

procedure cEmutecaVersionManager.AssingAllTo(aList: TStrings);
var
  i: longint;
begin
  if not assigned(aList) then
    aList := TStringList.Create;

  aList.BeginUpdate;
  i := 0;
  while i < FullList.Count do
  begin
    aList.AddObject(FullList[i].Title, FullList[i]);
    Inc(i);
  end;
  aList.EndUpdate;
end;

procedure cEmutecaVersionManager.AssingEnabledTo(aList: TStrings);
var
  i: longint;
begin
  if not assigned(aList) then
    aList := TStringList.Create;

  aList.BeginUpdate;
  i := 0;
  while i < EnabledList.Count do
  begin
    aList.AddObject(EnabledList[i].Title, EnabledList[i]);
    Inc(i);
  end;
  aList.EndUpdate;
end;

constructor cEmutecaVersionManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FFullList := cEmutecaVersionList.Create(True);
  FEnabledList := cEmutecaVersionList.Create(False);
  // TODO: OnCompare FullList.OnCompare := ;
end;

destructor cEmutecaVersionManager.Destroy;
begin
  FreeAndNil(FEnabledList);
  FreeAndNil(FFullList);
  inherited Destroy;
end;

procedure cEmutecaVersionManager.SetEnabledList(AValue: cEmutecaVersionList);
begin
  if FEnabledList=AValue then Exit;
  FEnabledList:=AValue;
end;

procedure cEmutecaVersionManager.SetFullList(AValue: cEmutecaVersionList);
begin
  if FFullList=AValue then Exit;
  FFullList:=AValue;
end;

procedure cEmutecaVersionManager.LoadFromFileTxt(TxtFile: TStrings);
var
  i: integer;
  TempVersion: cEmutecaVersion;
begin
  if not Assigned(TxtFile) then
    Exit;

  i := 1; // Skipping Header
  while i < TxtFile.Count do
  begin
    TempVersion := cEmutecaVersion.Create(nil);
    TempVersion.DataString := TxtFile[i];
    FullList.Add(TempVersion);
    Inc(i);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsLoadingVersionList, TempVersion.Title,
        TempVersion.Description, i, TxtFile.Count);
  end;

  EnabledList.Assign(FullList);
end;

procedure cEmutecaVersionManager.SaveToFileTxt(TxtFile: TStrings;
  const ExportMode: boolean);
var
  i: integer;
begin
  if not Assigned(TxtFile) then
    Exit;

  { TODO : cEmutecaVersionManager.SaveToFileTxt Export mode }
  TxtFile.Clear;
  TxtFile.Add('"ID","System","Parent","Title","Version","Folder","FileName"');

  i := 0;
  while i < FullList.Count do
  begin
    TxtFile.Add(FullList[i].DataString);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsSavingVersionList, FullList[i].System,
        FullList[i].Title, i + 1, FullList.Count);
    Inc(i);
  end;
end;

end.
