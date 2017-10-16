unit ufLEmuTKIcnSysCBX;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Types, StdCtrls, LCLType,
  ucCHXImageList, uCHXImageUtils,
  ucEmutecaSystem,
  ufEmutecaSystemCBX;

type

  { TfmLEmuTKIcnSysCBX }

  TfmLEmuTKIcnSysCBX = class(TfmEmutecaSystemCBX)
  private
    FDefSysIcon: TPicture;
    procedure SetDefSysIcon(AValue: TPicture);
  published
    property DefSysIcon: TPicture read FDefSysIcon write SetDefSysIcon;

    procedure cbxSystemDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKIcnSysCBX }

procedure TfmLEmuTKIcnSysCBX.SetDefSysIcon(AValue: TPicture);
begin
  if FDefSysIcon = AValue then
    Exit;
  FDefSysIcon := AValue;
end;

procedure TfmLEmuTKIcnSysCBX.cbxSystemDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  IconRect: TRect;
  aIcon: TPicture;
  aSystem: cEmutecaSystem;

begin
  if odInactive in State then
    Exit;

  aSystem := cEmutecaSystem(cbxSystem.Items.Objects[Index]);

  // Icon
  // aCBX.Canvas.FillRect(ARect);
  IconRect := ARect;
  IconRect.Left := IconRect.Left + 1;
  IconRect.Right := IconRect.Left + ARect.Bottom - ARect.Top;

  aIcon := nil;
  if assigned(aSystem) then
    aIcon := aSystem.Stats.Icon;

  if not assigned(aIcon) then
    aIcon := DefSysIcon;

  if assigned(aIcon) then
    cbxSystem.Canvas.StretchDraw(CorrectAspectRatio(IconRect, aIcon),
      aIcon.Graphic);

  // Text
  cbxSystem.Canvas.TextOut(IconRect.Right + 4, ARect.Top,
    cbxSystem.Items[Index]);
end;

end.
