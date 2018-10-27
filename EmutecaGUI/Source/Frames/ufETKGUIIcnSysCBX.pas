unit ufETKGUIIcnSysCBX;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Types, StdCtrls, LCLType,uCHXImageUtils,
  ucEmutecaSystem,
  ufEmutecaSystemCBX;

type

  { TfmETKGUIIcnSysCBX }

  TfmETKGUIIcnSysCBX = class(TfmEmutecaSystemCBX)
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

{ TfmETKGUIIcnSysCBX }

procedure TfmETKGUIIcnSysCBX.SetDefSysIcon(AValue: TPicture);
begin
  if FDefSysIcon = AValue then
    Exit;
  FDefSysIcon := AValue;
end;

procedure TfmETKGUIIcnSysCBX.cbxSystemDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  IconRect: TRect;
  aIcon: TPicture;
  aSystem: cEmutecaSystem;
  aCBXS: TComboBox;

begin
  if odInactive in State then
    Exit;

  if not (Control is TComboBox) then Exit;

  aCBXS := TComboBox(Control);

  aSystem := cEmutecaSystem(aCBXS.Items.Objects[Index]);

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
    aCBXS.Canvas.StretchDraw(CorrectAspectRatio(IconRect, aIcon), aIcon.Graphic);

  // Text
  aCBXS.Canvas.TextOut(IconRect.Right + 4, ARect.Top, aCBXS.Items[Index]);
end;

end.
