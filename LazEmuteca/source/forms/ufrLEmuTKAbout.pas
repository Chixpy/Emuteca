unit ufrLEmuTKAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ucCHXImageList,
  ufCHXAbout,
  ucEmuteca;

type

  { TfrmLEmuTKAbout }

  TfrmLEmuTKAbout = class(TfrmCHXAbout)
  private
    FEmuteca: cEmuteca;
    FCachedIcons: cCHXImageList;
    FVersionIcons: cCHXImageList;
    FZoneIcons: cCHXImageMap;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetCachedIcons(AValue: cCHXImageList);
    procedure SetVersionIcons(AValue: cCHXImageList);
    procedure SetZoneIcons(AValue: cCHXImageMap);

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
    property CachedIcons: cCHXImageList read FCachedIcons write SetCachedIcons;
    property ZoneIcons: cCHXImageMap read FZoneIcons write SetZoneIcons;
    property VersionIcons: cCHXImageList read FVersionIcons write SetVersionIcons;

    procedure UpdateInfo;

  end;

var
  frmLEmuTKAbout: TfrmLEmuTKAbout;

implementation

{$R *.lfm}

{ TfrmLEmuTKAbout }

procedure TfrmLEmuTKAbout.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;
end;

procedure TfrmLEmuTKAbout.SetCachedIcons(AValue: cCHXImageList);
begin
  if FCachedIcons = AValue then
    Exit;
  FCachedIcons := AValue;
end;

procedure TfrmLEmuTKAbout.SetVersionIcons(AValue: cCHXImageList);
begin
  if FVersionIcons = AValue then Exit;
  FVersionIcons := AValue;
end;

procedure TfrmLEmuTKAbout.SetZoneIcons(AValue: cCHXImageMap);
begin
  if FZoneIcons = AValue then Exit;
  FZoneIcons := AValue;
end;

procedure TfrmLEmuTKAbout.UpdateInfo;
begin
  mAditional.Clear;

  if assigned(Emuteca) then
  begin
    mAditional.Lines.Add('EMUTECA INFO');
    mAditional.Lines.Add(Format('Systems (Enabled/Total): %0:d / %1:d',
      [Emuteca.SystemManager.EnabledList.Count, Emuteca.SystemManager.FullList.Count]));
    mAditional.Lines.Add(Format('Emulators (Enabled/Total): %0:d / %1:d',
    [Emuteca.EmulatorManager.EnabledList.Count, Emuteca.EmulatorManager.FullList.Count]));
    mAditional.Lines.Add('');
  end;

  if assigned(CachedIcons) then
  begin
    mAditional.Lines.Add(Format('Cached icons: %0:d', [CachedIcons.Count]));
  end;

  if assigned(ZoneIcons) then
  begin
    mAditional.Lines.Add(Format('Zone icons: %0:d', [ZoneIcons.Count]));
  end;
  if assigned(VersionIcons) then
  begin
    mAditional.Lines.Add(Format('Version icons: %0:d', [VersionIcons.Count]));
  end;
end;

end.
