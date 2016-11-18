unit ucEmutecaSoftware;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8, contnrs,
  uCHXStrUtils,
  uaCHXStorable,
  ucEmutecaPlayingStats,
  ucEmutecaSystem, ucEmutecaGroup;

const
  // Constant for DumpStatus, fixed (for filenames)
  krsedsVerified = 'Verified';
  krsedsGood = 'GoodDump';
  krsedsAlternate = 'Alternate';
  krsedsOverDump = 'OverDump';
  krsedsBadDump = 'BadDump';
  krsedsUnderDump = 'UnderDump';

resourcestring
  // Strings for DumpStatus, translatable
  rsedsVerified = krsedsVerified;
  rsedsGood = krsedsGood;
  rsedsAlternate = krsedsAlternate;
  rsedsOverDump = krsedsOverDump;
  rsedsBadDump = krsedsBadDump;
  rsedsUnderDump = krsedsUnderDump;

type
  TEmutecaDumpStatus = (edsVerified, edsGood, edsAlternate, edsOverDump,
    edsBadDump, edsUnderDump);

const
  EmutecaDumpStatusKeys: array [TEmutecaDumpStatus] of string =
    ('!', '', 'a', 'o', 'b', 'u');
  //< Keys for DumpStatus
  EmutecaDumpStatusStrs: array [TEmutecaDumpStatus] of string =
    (rsedsVerified, rsedsGood, rsedsAlternate, rsedsOverDump,
    rsedsBadDump, rsedsUnderDump);
  //< Strings for DumpStatus (localizable)
  EmutecaDumpStatusStrsK: array [TEmutecaDumpStatus] of string =
    (krsedsVerified, krsedsGood, krsedsAlternate, krsedsOverDump,
    krsedsBadDump, krsedsUnderDump);
  //< Strings for DumpStatus (fixed, used for icon filenames, etc. )

type
  { cEmutecaSoftware. }

  cEmutecaSoftware = class(caCHXStorableTxt, IFPObserver)
  private
    FCracked: string;
    FDumpInfo: string;
    FDumpStatus: TEmutecaDumpStatus;
    FFileName: string;
    FFixed: string;
    FFolder: string;
    FHack: string;
    FID: string;
    FModified: string;
    FParent: cEmutecaGroup;
    FGroupKey: string;
    FPirate: string;
    FPublisher: string;
    FSortTitle: string;
    FStats: cEmutecaPlayingStats;
    FSystem: cEmutecaSystem;
    FSystemKey: string;
    FTitle: string;
    FTrainer: string;
    FTranslation: string;
    FTranslitTitle: string;
    FVersion: string;
    FYear: string;
    FZone: string;
    function GetDataString: string;
    procedure SetCracked(AValue: string);
    procedure SetDataString(AValue: string);
    procedure SetDumpInfo(AValue: string);
    procedure SetDumpStatus(AValue: TEmutecaDumpStatus);
    procedure SetFileName(AValue: string);
    procedure SetFixed(AValue: string);
    procedure SetFolder(AValue: string);
    procedure SetHack(AValue: string);
    procedure SetID(AValue: string);
    procedure SetModified(AValue: string);
    procedure SetGroup(AValue: cEmutecaGroup);
    procedure SetGroupKey(AValue: string);
    procedure SetPirate(AValue: string);
    procedure SetPublisher(AValue: string);
    procedure SetSortTitle(AValue: string);
    procedure SetStats(AValue: cEmutecaPlayingStats);
    procedure SetSystem(AValue: cEmutecaSystem);
    procedure SetSystemKey(AValue: string);
    procedure SetTitle(AValue: string);
    procedure SetTrainer(AValue: string);
    procedure SetTranslation(AValue: string);
    procedure SetTranslitTitle(AValue: string);
    procedure SetVersion(AValue: string);
    procedure SetYear(AValue: string);
    procedure SetZone(AValue: string);

  public
    property DataString: string read GetDataString write SetDataString;

    // Cached Data
    // -----------
    property System: cEmutecaSystem read FSystem write SetSystem;
    property Group: cEmutecaGroup read FParent write SetGroup;

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);
    {< Subject has changed. }

    procedure LoadFromFileTxt(TxtFile: TStrings); override;
    procedure SaveToFileTxt(TxtFile: TStrings; const ExportMode: boolean);
      override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    // Basic data
    // ----------
    property ID: string read FID write SetID;
    {< ID of the file. Usually SHA1, some system filename }
    property Folder: string read FFolder write SetFolder;
    {< Folder or archive where the file is in. }
    property FileName: string read FFileName write SetFileName;
    {< Filename (or file inside and archive).}
    property Title: string read FTitle write SetTitle;
    {< Title. }
    property GroupKey: string read FGroupKey write SetGroupKey;
    {< ID of the Group. }
    property SystemKey: string read FSystemKey write SetSystemKey;
    {< ID of the System. }

    // Additional title info
    // ---------------------
    property TranslitTitle: string
      read FTranslitTitle write SetTranslitTitle;
    {< Trasliterated name in english (ASCII7) characters. }
    property SortTitle: string read FSortTitle write SetSortTitle;
    {< Title formated for sorting purposes. }

    // Release data
    // ------------
    property Version: string read FVersion write SetVersion;
    {< Version (v1.0; PRG1) or release type info (demo, prototype),
         or both (v1.0RC2, alpha 3). }

    property Year: string read FYear write SetYear;
    {< Date of the release (no development one).

    Format: YYYY/MM/DD }

    property Publisher: string read FPublisher write SetPublisher;
    {< Publisher.

    Don't confuse with developer.}

    property Zone: string read FZone write SetZone;
    {< Zone. }

    // Version flags. (Based on Cowering + TOSEC)
    // ------------------------------------------
    property DumpStatus: TEmutecaDumpStatus
      read FDumpStatus write SetDumpStatus default edsGood;
    {< Merged Verified, good, bad dump, etc.}

    property DumpInfo: string read FDumpInfo write SetDumpInfo;
    {< Number of alternate, bad dump, etc. }

    property Fixed: string read FFixed write SetFixed;
    {< The game has been fixed to make it to work with emulators. }

    property Trainer: string read FTrainer write SetTrainer;
    {< The game has been modified to add cheats. }

    property Translation: string read FTranslation write SetTranslation;
    {< The game is fan-traslated.

        Lenguage abreviation of translation. }

    property Pirate: string read FPirate write SetPirate;
    {< The game was released physically breaking some IP laws.

       This is for not licensed copies of the games, mainly in China,
       Korea, etc. }

    property Cracked: string read FCracked write SetCracked;
    {< The game was modified for breaking some segurity check. }

    property Modified: string read FModified write SetModified;
    {< The game was modified by use (hiscores or saves). }

    property Hack: string read FHack write SetHack;
        {< The game was modified for changing something (intros, sprites).

          Only if not covered by previous properties.}

    // Usage statitics
    // ---------------
    property Stats: cEmutecaPlayingStats read FStats write SetStats;

  end;

  { cEmutecaSoftList }

  cEmutecaSoftList = TComponentList;

  TEmutecaReturnSoftCB = function(aSoft: cEmutecaSoftware): boolean of object;

function Key2EmutecaDumpSt(aString: string): TEmutecaDumpStatus;
// Result := EmutecaDumpStatusStrs[DumpStatus];
// Result := EmutecaDumpStatusKeys[DumpStatus];

implementation

function Key2EmutecaDumpSt(aString: string): TEmutecaDumpStatus;
begin
  aString := UTF8Trim(UTF8LowerString(aString));

  if (aString = EmutecaDumpStatusKeys[edsGood]) then // krsedsGoodKey = ''
    Result := edsGood
  else if (aString[1] = EmutecaDumpStatusKeys[edsVerified]) then
    Result := edsVerified
  else if (aString[1] = EmutecaDumpStatusKeys[edsAlternate]) then
    Result := edsAlternate
  else if (aString[1] = EmutecaDumpStatusKeys[edsOverDump]) then
    Result := edsOverDump
  else if (aString[1] = EmutecaDumpStatusKeys[edsBadDump]) then
    Result := edsBadDump
  else if (aString[1] = EmutecaDumpStatusKeys[edsUnderDump]) then
    Result := edsUnderDump
  else
    Result := edsGood;
end;

{ cEmutecaSoftware }

function cEmutecaSoftware.GetDataString: string;
var
  aStringList: TStringList;
begin
  aStringList := TStringList.Create;
  try
    SaveToFileTxt(aStringList, False);
  finally
    Result := aStringList.CommaText;
    FreeAndNil(aStringList);
  end;
end;

procedure cEmutecaSoftware.SetCracked(AValue: string);
begin
  if FCracked = AValue then
    Exit;
  FCracked := AValue;
end;

procedure cEmutecaSoftware.SetDataString(AValue: string);
var
  aStringList: TStringList;
begin
  aStringList := TStringList.Create;
  try
    aStringList.CommaText := AValue;

    LoadFromFileTxt(aStringList);
  finally
    FreeAndNil(aStringList);
  end;
end;

procedure cEmutecaSoftware.SetDumpInfo(AValue: string);
begin
  if FDumpInfo = AValue then
    Exit;
  FDumpInfo := AValue;
end;

procedure cEmutecaSoftware.SetDumpStatus(AValue: TEmutecaDumpStatus);
begin
  if FDumpStatus = AValue then
    Exit;
  FDumpStatus := AValue;
end;

procedure cEmutecaSoftware.SetVersion(AValue: string);
begin
  if FVersion = AValue then
    Exit;
  FVersion := AValue;
end;

procedure cEmutecaSoftware.SetYear(AValue: string);
begin
  if FYear = AValue then
    Exit;
  FYear := AValue;
end;

procedure cEmutecaSoftware.SetZone(AValue: string);
begin
  if FZone = AValue then
    Exit;
  FZone := AValue;
end;

procedure cEmutecaSoftware.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if not assigned(ASender) then
    Exit;
  if ASender = System then
  begin
    case Operation of
      ooFree: System := nil;
      else
        SystemKey := cEmutecaSystem(ASender).ID;
    end;
  end
  else if ASender = Group then
  begin
    case Operation of
      ooFree: Group := nil;
      else
        SystemKey := cEmutecaGroup(ASender).ID;
    end;
  end;
end;

procedure cEmutecaSoftware.SetSortTitle(AValue: string);
begin
  FSortTitle := UTF8LowerString(AValue);
end;

procedure cEmutecaSoftware.SetStats(AValue: cEmutecaPlayingStats);
begin
  if FStats = AValue then
    Exit;
  FStats := AValue;
end;

procedure cEmutecaSoftware.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;

  if Assigned(FSystem) then
    FSystem.FPODetachObserver(Self);

  FSystem := AValue;

  if Assigned(System) then
    System.FPOAttachObserver(Self);
end;

procedure cEmutecaSoftware.SetTranslitTitle(AValue: string);
begin
  if FTranslitTitle = AValue then
    Exit;
  FTranslitTitle := AValue;
end;

procedure cEmutecaSoftware.SetFileName(AValue: string);
begin
  FFileName := SetAsFile(AValue);
end;

procedure cEmutecaSoftware.SetFixed(AValue: string);
begin
  if FFixed = AValue then
    Exit;
  FFixed := AValue;
end;

procedure cEmutecaSoftware.SetID(AValue: string);
begin
  FID := SetAsID(AValue);
end;

procedure cEmutecaSoftware.SetModified(AValue: string);
begin
  if FModified = AValue then
    Exit;
  FModified := AValue;
end;

procedure cEmutecaSoftware.SetGroup(AValue: cEmutecaGroup);
begin
  if FParent = AValue then
    Exit;

  if Assigned(FParent) then
    FParent.FPODetachObserver(Self);

  FParent := AValue;

  if Assigned(Group) then
    Group.FPOAttachObserver(Self);
end;

procedure cEmutecaSoftware.SetGroupKey(AValue: string);
begin
  FGroupKey := SetAsID(AValue);
end;

procedure cEmutecaSoftware.SetPirate(AValue: string);
begin
  if FPirate = AValue then
    Exit;
  FPirate := AValue;
end;

procedure cEmutecaSoftware.SetPublisher(AValue: string);
begin
  if FPublisher = AValue then
    Exit;
  FPublisher := AValue;
end;

procedure cEmutecaSoftware.SetFolder(AValue: string);
begin
  FFolder := SetAsFolder(AValue);
end;

procedure cEmutecaSoftware.SetHack(AValue: string);
begin
  if FHack = AValue then
    Exit;
  FHack := AValue;
end;

procedure cEmutecaSoftware.SetSystemKey(AValue: string);
begin
  FSystemKey := AValue;
end;

procedure cEmutecaSoftware.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
end;

procedure cEmutecaSoftware.SetTrainer(AValue: string);
begin
  if FTrainer = AValue then
    Exit;
  FTrainer := AValue;
end;

procedure cEmutecaSoftware.SetTranslation(AValue: string);
begin
  if FTranslation = AValue then
    Exit;
  FTranslation := AValue;
end;

constructor cEmutecaSoftware.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FStats := cEmutecaPlayingStats.Create(Self);
end;

destructor cEmutecaSoftware.Destroy;
begin
  if Assigned(Group) then
    Group.FPODetachObserver(Self);
  if Assigned(System) then
    System.FPODetachObserver(Self);
  FreeAndNil(FStats);

  inherited Destroy;
end;

procedure cEmutecaSoftware.LoadFromFileTxt(TxtFile: TStrings);
var
  i: integer;
begin
  if not assigned(TxtFile) then
    Exit;

  i := 0;
  while i < TxtFile.Count do
  begin
    case i of
      0: ID := TxtFile[i];
      1: Folder := TxtFile[i];
      2: FileName := TxtFile[i];
      3: Title := TxtFile[i];
      4: GroupKey := TxtFile[i];
      5: SystemKey := TxtFile[i];
      // 6: ;
      7: TranslitTitle := TxtFile[i];
      8: SortTitle := TxtFile[i];
      // 9: ;
      10: Version := TxtFile[i];
      11: Year := TxtFile[i];
      12: Publisher := TxtFile[i];
      13: Zone := TxtFile[i];
      // 14: ;
      15: DumpStatus := Key2EmutecaDumpSt(TxtFile[i]);
      16: DumpInfo := TxtFile[i];
      17: Fixed := TxtFile[i];
      18: Trainer := TxtFile[i];
      19: Translation := TxtFile[i];
      20: Pirate := TxtFile[i];
      21: Cracked := TxtFile[i];
      22: Modified := TxtFile[i];
      23: Hack := TxtFile[i];
      // 24: ;
      25: Stats.LastTime := StrToFloatDef(TxtFile[i], 0);
      26: Stats.TimesPlayed := StrToIntDef(TxtFile[i], 0);
      27: Stats.PlayingTime := StrToCardinalDef(TxtFile[i], 0);
      else
        ;
    end;
    Inc(i);
  end;
end;

procedure cEmutecaSoftware.SaveToFileTxt(TxtFile: TStrings;
  const ExportMode: boolean);
begin
  if not assigned(TxtFile) then
    Exit;

  // Basic data
  // ----------
  TxtFile.Add(ID);
  TxtFile.Add(Folder);
  TxtFile.Add(FileName);
  TxtFile.Add(Title);
  TxtFile.Add(GroupKey);
  TxtFile.Add(SystemKey);
  TxtFile.Add(''); // Reserved

  // Additional title info
  // ---------------------
  TxtFile.Add(TranslitTitle);
  TxtFile.Add(SortTitle);
  TxtFile.Add(''); // Reserved

  // Release data
  // ------------
  TxtFile.Add(Version);
  TxtFile.Add(Year);
  TxtFile.Add(Publisher);
  TxtFile.Add(Zone);
  TxtFile.Add(''); // Reserved

  // Version Flags
  // ---------------
  TxtFile.Add(EmutecaDumpStatusKeys[DumpStatus]);
  TxtFile.Add(DumpInfo);
  TxtFile.Add(Fixed);
  TxtFile.Add(Trainer);
  TxtFile.Add(Translation);
  TxtFile.Add(Pirate);
  TxtFile.Add(Cracked);
  TxtFile.Add(Modified);
  TxtFile.Add(Hack);
  TxtFile.Add(''); // Reserved

  // Usage statitics
  // ---------------
  TxtFile.Add(FloatToStr(Stats.LastTime));
  TxtFile.Add(IntToStr(Stats.TimesPlayed));
  TxtFile.Add(IntToStr(Stats.PlayingTime));
end;

initialization
  RegisterClass(cEmutecaSoftware);

finalization
  UnRegisterClass(cEmutecaSoftware);
end.
