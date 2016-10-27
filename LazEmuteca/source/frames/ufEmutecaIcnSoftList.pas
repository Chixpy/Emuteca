unit ufEmutecaIcnSoftList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  VirtualTrees, LCLIntf, LCLType,
  ucCHXImageList, uCHXImageUtils,
  ucEmutecaSoftware,
  ufEmutecaSoftList;

type

  { TfmEmutecaIcnSoftList }

  TfmEmutecaIcnSoftList = class(TfmEmutecaSoftList)
    procedure VSTDrawText(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellText: string; const CellRect: TRect;
      var DefaultDraw: boolean);
  private
    FDumpIconList: cCHXImageList;
    FSoftIconList: cCHXImageList;
    procedure SetDumpIconList(AValue: cCHXImageList);
    procedure SetSoftIconList(AValue: cCHXImageList);
    { private declarations }
  public
    { public declarations }

    property SoftIconList: cCHXImageList
      read FSoftIconList write SetSoftIconList;
    //< Game icons (exclusive ones).
    property DumpIconList: cCHXImageList
      read FDumpIconList write SetDumpIconList;
    {< Icons of dump info. }
  end;

implementation

{$R *.lfm}

{ TfmEmutecaIcnSoftList }

procedure TfmEmutecaIcnSoftList.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: string; const CellRect: TRect; var DefaultDraw: boolean);
var
  Data: ^cEmutecaSoftware;
  IconRect: TRect;
begin
  DefaultDraw := True;



  case Column of
    1: // Title
    begin
      if not assigned(SoftIconList) then
        Exit;

      if (Node = nil) then
        Exit;
      Data := VST.GetNodeData(Node);
      if (Data^ = nil) then
        Exit;

      DefaultDraw := False;

      // Icon space
      IconRect := CellRect;
      IconRect.Right := IconRect.Left + IconRect.Bottom - IconRect.Top;

      if Data^.Stats.IconIndex = -1 then
      begin

      end;

      if (Data^.Stats.IconIndex > -1) and
        (Data^.Stats.IconIndex < SoftIconList.Count) then
        TargetCanvas.StretchDraw(CorrectAspectRatio(IconRect,
          SoftIconList[Data^.Stats.IconIndex]),
          SoftIconList[Data^.Stats.IconIndex].Graphic);

      IconRect := CellRect;
      IconRect.Left := IconRect.Left + IconRect.Bottom -
        IconRect.Top + VST.TextMargin;

      DrawText(TargetCanvas.Handle, PChar(CellText), -1, IconRect,
        DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE or
        DT_WORDBREAK or DT_END_ELLIPSIS or DT_EDITCONTROL);

    end;

    5: // Flags
    begin
      if not assigned(DumpIconList) then
        Exit;

      if (Node = nil) then
        Exit;
      Data := VST.GetNodeData(Node);
      if (Data^ = nil) then
        Exit;

      DefaultDraw := False;

      // Icon space
      IconRect := CellRect;
      IconRect.Right := IconRect.Left + IconRect.Bottom - IconRect.Top;

      TargetCanvas.StretchDraw(CorrectAspectRatio(IconRect,
        DumpIconList[Ord(Data^.DumpStatus)]),
        DumpIconList[Ord(Data^.DumpStatus)].Graphic);

      // Next position
      IconRect.Left := IconRect.Left + IconRect.Bottom - IconRect.Top;
      IconRect.Right := IconRect.Right + IconRect.Bottom - IconRect.Top;
    end;
    else
      DefaultDraw := True;
  end;
end;

procedure TfmEmutecaIcnSoftList.SetSoftIconList(AValue: cCHXImageList);
begin
  if FSoftIconList = AValue then
    Exit;
  FSoftIconList := AValue;
end;

procedure TfmEmutecaIcnSoftList.SetDumpIconList(AValue: cCHXImageList);
begin
  if FDumpIconList = AValue then
    Exit;
  FDumpIconList := AValue;
end;

end.
