unit ucEmutecaSoftware;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazFileUtils, LazUTF8, contnrs,
  uCHXStrUtils,
  uaEmutecaStorable,
  ucEmutecaPlayingStats;

const
  // Constants for DumpStatus
  krsedsVerifiedKey = '!';
  krsedsGoodKey = '';
  krsedsAlternateKey = 'a';
  krsedsOverDumpKey = 'o';
  krsedsBadDumpKey = 'b';
  krsedsUnderDumpKey = 'u';

resourcestring
  // Strings for DumpStatus
  rsedsVerified = 'Verified';
  rsedsGood = 'Good';
  rsedsAlternate = 'Alternate';
  rsedsOverDump = 'OverDump';
  rsedsBadDump = 'BadDump';
  rsedsUnderDump = 'UnderDump';

type
  TEmutecaDumpStatus = (edsVerified, edsGood, edsAlternate, edsOverDump,
    edsBadDump, edsUnderDump);

  { cEmutecaSoftware. }

  cEmutecaSoftware = class(caEmutecaStorableTxt)
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
    FParent: string;
    FPirate: string;
    FPublisher: string;
    FSortTitle: string;
    FStats: cEmutecaPlayingStats;
    FSystem: string;
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
    procedure SetParent(AValue: string);
    procedure SetPirate(AValue: string);
    procedure SetPublisher(AValue: string);
    procedure SetSortTitle(AValue: string);
    procedure SetStats(AValue: cEmutecaPlayingStats);
    procedure SetSystem(AValue: string);
    procedure SetTitle(AValue: string);
    procedure SetTrainer(AValue: string);
    procedure SetTranslation(AValue: string);
    procedure SetTranslitTitle(AValue: string);
    procedure SetVersion(AValue: string);
    procedure SetYear(AValue: string);
    procedure SetZone(AValue: string);

  public
    property DataString: string read GetDataString write SetDataString;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromFileTxt(TxtFile: TStrings); override;
    procedure SaveToFileTxt(TxtFile: TStrings; const ExportMode: boolean);
      override;

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
    property Parent: string read FParent write SetParent;
    {< ID of the parent. }
    property System: string read FSystem write SetSystem;
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
      read FDumpStatus write SetDumpStatus;
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

function EmutecaDumpSt2Str(aEDS: TEmutecaDumpStatus): string;
function Str2EmutecaDumpSt(aString: string): TEmutecaDumpStatus;

implementation

function EmutecaDumpSt2Str(aEDS: TEmutecaDumpStatus): string;
begin
    case aEDS of
    edsVerified: Result := krsedsVerifiedKey;
    edsGood: Result := krsedsGoodKey;
    edsAlternate: Result := krsedsAlternateKey;
    edsOverDump: Result := krsedsOverDumpKey;
    edsBadDump: Result := krsedsBadDumpKey;
    edsUnderDump: Result := krsedsUnderDumpKey;
    else
      Result := krsedsGoodKey;
  end;
end;

function Str2EmutecaDumpSt(aString: string): TEmutecaDumpStatus;
begin
    if (aString = krsedsGoodKey) then // krsedsGoodKey = ''
    Result := edsGood
  else
    case aString[1] of
      krsedsVerifiedKey: Result := edsVerified;
      krsedsAlternateKey: Result := edsAlternate;
      krsedsOverDumpKey: Result := edsOverDump;
      krsedsBadDumpKey: Result := edsBadDump;
      krsedsUnderDumpKey: Result := edsUnderDump;
      else
        Result := edsGood;
    end;
{
  if (aString = krsedsGoodKey) then // krsedsGoodKey = ''
    Result := edsGood
  else if (aString[1] = krsedsVerifiedKey) then
    Result := edsVerified
  else if (aString[1] = krsedsAlternateKey) then
    Result := edsAlternate
  else if (aString[1] = krsedsOverDumpKey) then
    Result := edsOverDump
  else if (aString[1] = krsedsBadDumpKey) then
    Result := edsBadDump
  else if (aString[1] = krsedsUnderDumpKey) then
    Result := edsUnderDump
  else
    Result := edsGood;
    }
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

procedure cEmutecaSoftware.SetParent(AValue: string);
begin
  FParent := SetAsID(AValue);
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

procedure cEmutecaSoftware.SetSystem(AValue: string);
begin
  FSystem := SetAsID(AValue);
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
      4: Parent := TxtFile[i];
      5: System := TxtFile[i];
      // 6: ;
      7: TranslitTitle := TxtFile[i];
      8: SortTitle := TxtFile[i];
      // 9: ;
      10: Version := TxtFile[i];
      11: Year := TxtFile[i];
      12: Publisher := TxtFile[i];
      13: Zone := TxtFile[i];
      // 14: ;
      15: DumpStatus := Str2EmutecaDumpSt(TxtFile[i]);
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
  TxtFile.Add(Parent);
  TxtFile.Add(System);
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
  TxtFile.Add(EmutecaDumpSt2Str(DumpStatus));
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
