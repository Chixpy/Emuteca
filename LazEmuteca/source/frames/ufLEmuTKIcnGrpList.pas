unit ufLEmuTKIcnGrpList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  VirtualTrees, LCLIntf, LCLType,
  ucCHXImageList, uCHXImageUtils, //uCHXStrUtils,
  ucEmuteca, ucEmutecaGroup,
  ufEmutecaGroupList,
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

    property GroupIconList: cCHXImageList read FGroupIconList write SetGroupIconList;

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
  Data: ^cEmutecaGroup;
  IconRect: TRect;
  aIcon: TPicture;
  TmpStr: string;
begin
  DefaultDraw := True;

  case Column of
    0: // Title
    begin
      if not assigned(GroupIconList) then
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

  //    if Data^.Stats.IconIndex = -1 then
  //    begin
  //      TmpStr :=
  //        Emuteca.SearchFirstGroupFile(Data^.System.IconFolder,
  //        Data^, GUIConfig.ImageExtensions);
  //      if TmpStr = '' then
  //        Data^.Stats.IconIndex := 0
  //      else
  //        Data^.Stats.IconIndex := GroupIconList.AddImageFile(TmpStr);
  //    end;

  //    if (Data^.Stats.IconIndex < GroupIconList.Count) then
   //   begin
   //     aIcon := GroupIconList[Data^.Stats.IconIndex];
    //    TargetCanvas.StretchDraw(CorrectAspectRatio(IconRect, aIcon),
   //       aIcon.Graphic);
    //  end;

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

   //ReadActionsIcons(GUIConfig.GUIIcnFile, Self.Name, ilSoftList, alSoftList);
end;

constructor TfmLEmuTKIcnGrpList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmLEmuTKIcnGrpList.Destroy;
begin
  inherited Destroy;
end;

end.
