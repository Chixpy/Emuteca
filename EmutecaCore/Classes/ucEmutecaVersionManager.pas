unit ucEmutecaVersionManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8,
  uEmutecaCommon, ucEmutecaVersion;

type

  { cEmutecaVersionManager }

  cEmutecaVersionManager = class(TComponent)
  private
    FCurrentList: cEmutecaVersionList;
    FDataFile: string;
    FProgressCallBack: TEmutecaProgressCallBack;
    FList: cEmutecaVersionList;
    procedure SetDataFile(AValue: string);
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack);

  protected


  public
    property List: cEmutecaVersionList read FList;
    {< Actual list where the version are stored. }
    property CurrentList: cEmutecaVersionList read FCurrentList;
    {< Current loaded games list. }

    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressCallBack;

    procedure LoadDataFile;
    // Loads current emulators file
    procedure SaveDataFile;
    // Saves current emulators file

    procedure ImportDataFile(const aFileName: string);
    // Imports data to the current list
    procedure ExportDataFile(const aFileName: string);
    // Exports data of the current list

    function ItemById(aId: string): cEmutecaVersion;
    //< Return the version with have aId key

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property DataFile: string read FDataFile write SetDataFile;
  end;

implementation

{ cEmutecaVersionManager }

procedure cEmutecaVersionManager.SetProgressCallBack(
  AValue: TEmutecaProgressCallBack);
begin
  if FProgressCallBack = AValue then
    Exit;
  FProgressCallBack := AValue;
end;

procedure cEmutecaVersionManager.LoadDataFile;
begin
  List.Clear;
  ImportDataFile(DataFile); // Dirty trick :-P
end;

procedure cEmutecaVersionManager.SaveDataFile;
begin
    List.SaveToFile(DataFile);
end;

procedure cEmutecaVersionManager.ImportDataFile(const aFileName: string);
var
  OldCF: string;
  i: Integer;
begin
  OldCF := DataFile;
  DataFile := aFileName;
  List.LoadFromFile(DataFile);
  // As List changed, then add all games to visible list
  CurrentList.Clear;
  i := 0;
  while i < List.count do
  begin
     CurrentList.Add(List[i]);
     inc(i);
  end;
  DataFile := OldCF;
end;

procedure cEmutecaVersionManager.ExportDataFile(const aFileName: string);
var
  OldCF: string;
begin
  OldCF := DataFile;
  DataFile := aFilename;
  { TODO : Don't save user data... (Path to the File and if it's enabled) }
  SaveDataFile;
  DataFile := OldCF;
end;

function cEmutecaVersionManager.ItemById(aId: string): cEmutecaVersion;
var
  i: integer;
  aItem: cEmutecaVersion;
begin
  Result := nil;
  aId := Trim(UTF8LowerCase(aId));

  // Maybe backwards is better for batch operations...
  i := List.Count - 1;
  while (i >= 0) do
  begin
    aItem := List.Items[i];
    if UTF8CompareText(aItem.ID, aId) = 0 then
    begin
      Result := aItem;
      Break; // ... dirty exit, but we don't need to check: Result <> nil
    end;
    Dec(i);
  end;
end;

constructor cEmutecaVersionManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FList := cEmutecaVersionList.Create(True);
  FCurrentList := cEmutecaVersionList.Create(False);
end;

destructor cEmutecaVersionManager.Destroy;
begin
  SaveDataFile;
  FreeAndNil(FCurrentList);
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure cEmutecaVersionManager.SetDataFile(AValue: string);
begin
  if FDataFile = AValue then Exit;
  FDataFile := AValue;
end;

end.
