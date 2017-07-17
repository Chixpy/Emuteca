unit ufLEmuTKIcnGrpList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  VirtualTrees, LCLIntf, LCLType, LazUTF8, LazFileUtils,
  ucCHXImageList, uCHXImageUtils, //uCHXStrUtils,
  ucEmuteca, ucEmutecaSystem, ucEmutecaGroup,
  ufEmutecaGroupListOld,
  uGUIConfig;

type

  { TfmLEmuTKIcnGrpList }

  TfmLEmuTKIcnGrpList = class(TfmEmutecaGroupList)
    procedure VSTDrawText(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellText: string; const CellRect: TRect; var DefaultDraw: boolean);
  private
    FEmuteca: cEmuteca;
    FGroupIconList: cCHXImageList;
    FGUIConfig: cGUIConfig;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetGroupIconList(AValue: cCHXImageList);
    procedure SetGUIConfig(AValue: cGUIConfig);

  public
    property GUIConfig: cGUIConfig read FGUIConfig write SetGUIConfig;
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    property GroupIconList: cCHXImageList
      read FGroupIconList write SetGroupIconList;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKIcnGrpList }

procedure TfmLEmuTKIcnGrpList.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: string; const CellRect: TRect; var DefaultDraw: boolean);
var
  pData: ^cEmutecaGroup;
  IconRect: TRect;
  aIcon: TPicture;
  TmpStr: string;
begin
  DefaultDraw := True;

  if not Assigned(GroupIconList) then
    Exit;
  if not Assigned(Emuteca) then
    Exit;

  case Column of
    0: // System
    begin
      pData := VST.GetNodeData(Node);
      if (pData^ = nil) then
        Exit;

      DefaultDraw := False;

      // Icon space
      IconRect := CellRect;
      IconRect.Right := IconRect.Left + IconRect.Bottom - IconRect.Top;


      // TODO: Make it simple, Emuteca or System will search the icon
      if pData^.CachedSystem.Stats.IconIndex = -1 then
      begin
        if FileExistsUTF8(pData^.CachedSystem.Icon) then
          pData^.CachedSystem.Stats.IconIndex :=
            GroupIconList.AddImageFile(pData^.CachedSystem.Icon)
        else
          pData^.CachedSystem.Stats.IconIndex := 0;
      end;

      if (pData^.CachedSystem.Stats.IconIndex < GroupIconList.Count) then
      begin
        aIcon := GroupIconList[pData^.CachedSystem.Stats.IconIndex];
        TargetCanvas.StretchDraw(CorrectAspectRatio(IconRect, aIcon),
          aIcon.Graphic);
      end;

      // Don't draw text

      // Text space
      //  IconRect := CellRect;
      //  IconRect.Left := IconRect.Left + IconRect.Bottom -
      //  IconRect.Top + VST.TextMargin;

      // DrawText(TargetCanvas.Handle, PChar(CellText), -1, IconRect,
      //  DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE or
      //  DT_WORDBREAK or DT_END_ELLIPSIS or DT_EDITCONTROL);
    end;

    1: // Title
    begin
      pData := VST.GetNodeData(Node);
      if (pData^ = nil) then
        Exit;

      DefaultDraw := False;

      // Icon space
      IconRect := CellRect;
      IconRect.Right := IconRect.Left + IconRect.Bottom - IconRect.Top;

      // TODO: Make it simple, Emuteca or System will search the icon
      if pData^.Stats.IconIndex = -1 then
      begin
        TmpStr := Emuteca.SearchFirstGroupFile(pData^.CachedSystem.IconFolder,
          pData^, GUIConfig.ImageExtensions);

        if TmpStr = '' then
          pData^.Stats.IconIndex := 0
        else
          pData^.Stats.IconIndex := GroupIconList.AddImageFile(TmpStr);
      end;

      if (pData^.Stats.IconIndex < GroupIconList.Count) then
      begin
        aIcon := GroupIconList[pData^.Stats.IconIndex];
        TargetCanvas.StretchDraw(CorrectAspectRatio(IconRect, aIcon),
          aIcon.Graphic);
      end;

      // Text space
      IconRect := CellRect;
      IconRect.Left := IconRect.Left + IconRect.Bottom -
        IconRect.Top + VST.TextMargin;

      DrawText(TargetCanvas.Handle, PChar(CellText), -1, IconRect,
        DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE or
        DT_WORDBREAK or DT_END_ELLIPSIS or DT_EDITCONTROL);
    end;

  end;
end;

procedure TfmLEmuTKIcnGrpList.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;
end;

procedure TfmLEmuTKIcnGrpList.SetGroupIconList(AValue: cCHXImageList);
begin
  if FGroupIconList = AValue then
    Exit;
  FGroupIconList := AValue;
end;

procedure TfmLEmuTKIcnGrpList.SetGUIConfig(AValue: cGUIConfig);
begin
  if FGUIConfig = AValue then
    Exit;
  FGUIConfig := AValue;

  //ReadActionsIcons(GUIConfig.GUIIcnFile, Name, ilSoftList, alSoftList);
end;

constructor TfmLEmuTKIcnGrpList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  // Set Width of icon columns
  // System
  vst.Header.Columns[0].Width :=
    vst.DefaultNodeHeight + vst.Header.Columns[0].Spacing * 2 +
    vst.Header.Columns[0].Margin * 2;
end;

destructor TfmLEmuTKIcnGrpList.Destroy;
begin
  inherited Destroy;
end;

end.
