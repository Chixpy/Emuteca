unit ufEmutecaIcnSoftList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  VirtualTrees, LCLIntf, LCLType,
  ucCHXImageList, uCHXImageUtils,
  ucEmutecaPlayingStats,
  ufEmutecaSoftList;

type

  { TfmEmutecaIcnSoftList }

  TfmEmutecaIcnSoftList = class(TfmEmutecaSoftList)
    procedure VSTDrawText(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellText: string; const CellRect: TRect;
      var DefaultDraw: boolean);
  private
    FIconList: cCHXImageList;
    procedure SetIconList(AValue: cCHXImageList);
    { private declarations }
  public
    { public declarations }

    property IconList: cCHXImageList read FIconList write SetIconList;
    //< Game icons (exclusive ones).
  end;

implementation

{$R *.lfm}

{ TfmEmutecaIcnSoftList }

procedure TfmEmutecaIcnSoftList.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: string; const CellRect: TRect; var DefaultDraw: boolean);
var
  Data: ^cEmutecaPlayingStats;
  IconRect: TRect;
begin
  DefaultDraw := True;

   if not assigned(IconList) then
    Exit;

  if (Node = nil) then
    Exit;
  Data := VST.GetNodeData(Node);
  if (Data^ = nil) then
    Exit;

  case Column of
    1: // Title
    begin
      DefaultDraw := False;

      // Icon space
      IconRect := CellRect;
      IconRect.Right := IconRect.Left + IconRect.Bottom - IconRect.Top;

      if Data^.IconIndex = -1 then
      begin

      end;

      if (Data^.IconIndex > -1) and (Data^.IconIndex <
        IconList.Count) then
        TargetCanvas.StretchDraw(CorrectAspetRatio(IconRect,
          IconList[Data^.IconIndex]),
          IconList[Data^.IconIndex].Graphic);

      IconRect := CellRect;
      IconRect.Left := IconRect.Left + IconRect.Bottom - IconRect.Top +
        VST.TextMargin;

      DrawText(TargetCanvas.Handle, PChar(CellText), -1, IconRect,
        DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE or
        DT_WORDBREAK or DT_END_ELLIPSIS or DT_EDITCONTROL);

    end;
    5: // Flags
    begin
      DefaultDraw := False;

      // Icon space
      IconRect := CellRect;
      IconRect.Right := IconRect.Left + IconRect.Bottom - IconRect.Top;
    end;
    else
      DefaultDraw := True;
  end;
end;

procedure TfmEmutecaIcnSoftList.SetIconList(AValue: cCHXImageList);
begin
  if FIconList = AValue then Exit;
  FIconList := AValue;
end;

end.
