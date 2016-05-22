unit uEmutecaVersionFlags;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { cEmutecaVersionFlags }

  cEmutecaVersionFlags = class(TComponent)
  private
    FAlternate: string;
    FBadDump: string;
    FCracked: string;
    FFixed: string;
    FHack: string;
    FModified: string;
    FPirate: string;
    FTrainer: string;
    FTranslation: string;
    FVerified: boolean;
    procedure SetAlternate(AValue: string);
    procedure SetBadDump(AValue: string);
    procedure SetCracked(AValue: string);
    procedure SetFixed(AValue: string);
    procedure SetHack(AValue: string);
    procedure SetModified(AValue: string);
    procedure SetPirate(AValue: string);
    procedure SetTrainer(AValue: string);
    procedure SetTranslation(AValue: string);
    procedure SetVerified(AValue: boolean);

  protected

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    // Emuteca flags (based on Cowering/TOSEC)
    // --------------------

    property Verified: boolean read FVerified write SetVerified;
    {< Verified good dump. }

    property Alternate: string read FAlternate write SetAlternate;
      {< Alternate dump (of same version).

        Usually have an ID number, but its better to have the main difference.}

    property BadDump: string read FBadDump write SetBadDump;
      {< Bad dump, underdump or overdump.

      @definitionList(
      @itemLabel(Bad dumps)
      @item(ID number or problem.)
      @itemLabel(Underdump)
      @item('-' + ID number or problem.)
      @itemLabel(Overdump)
      @item('+' + ID number or problem.)
      )}

    property Fixed: string read FFixed write SetFixed;
    {< The game has been fixed to make it to work with emulators. }

    property Trainer: string read FTrainer write SetTrainer;
    {< The game has been modified to add cheats. }

    property Translation: string read FTranslation write SetTranslation;
      {< The game is fan-traslated.

      Lenguage abreviation of translation.}

    property Pirate: string read FPirate write SetPirate;
      {< The game was released physically breaking some IP laws.

      This is for not licensed copies of the games, mainly in China, Korea, etc.
      }

    property Cracked: string read FCracked write SetCracked;
    {< The game was modified for breaking some segurity check. }

    property Modified: string read FModified write SetModified;
    {< The game was modified by use (hiscores or saves). }

    property Hack: string read FHack write SetHack;
      {< The game was modified for changing something (intros, sprites).

        Only if not covered by previous properties.}
  end;

implementation

{ cEmutecaVersionFlags }

procedure cEmutecaVersionFlags.SetAlternate(AValue: string);
begin
  if FAlternate = AValue then
    Exit;
  FAlternate := AValue;
end;

procedure cEmutecaVersionFlags.SetBadDump(AValue: string);
begin
  if FBadDump = AValue then
    Exit;
  FBadDump := AValue;
end;

procedure cEmutecaVersionFlags.SetCracked(AValue: string);
begin
  if FCracked = AValue then
    Exit;
  FCracked := AValue;
end;

procedure cEmutecaVersionFlags.SetFixed(AValue: string);
begin
  if FFixed = AValue then
    Exit;
  FFixed := AValue;
end;

procedure cEmutecaVersionFlags.SetHack(AValue: string);
begin
  if FHack = AValue then
    Exit;
  FHack := AValue;
end;

procedure cEmutecaVersionFlags.SetModified(AValue: string);
begin
  if FModified = AValue then
    Exit;
  FModified := AValue;
end;

procedure cEmutecaVersionFlags.SetPirate(AValue: string);
begin
  if FPirate = AValue then
    Exit;
  FPirate := AValue;
end;

procedure cEmutecaVersionFlags.SetTrainer(AValue: string);
begin
  if FTrainer = AValue then
    Exit;
  FTrainer := AValue;
end;

procedure cEmutecaVersionFlags.SetTranslation(AValue: string);
begin
  if FTranslation = AValue then
    Exit;
  FTranslation := AValue;
end;

procedure cEmutecaVersionFlags.SetVerified(AValue: boolean);
begin
  if FVerified = AValue then
    Exit;
  FVerified := AValue;
end;

constructor cEmutecaVersionFlags.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor cEmutecaVersionFlags.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(cEmutecaVersionFlags);

finalization
  UnRegisterClass(cEmutecaVersionFlags);

end.
