unit ufLEmuTKIcnSoftList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls,
  Graphics, Dialogs,
  VirtualTrees, LCLIntf, LCLType, LazUTF8,
  ucCHXImageList, uCHXImageUtils, uCHXStrUtils,
  ucEmuteca,
  ucEmutecaGroup, ucEmutecaSoftware,
  ufEmutecaSoftList,
  uGUIConfig;

const
  LazEmuTKIconFiles: array [0..12] of string =
    (krsedsVerified, krsedsGood, krsedsAlternate, krsedsOverDump,
    krsedsBadDump, krsedsUnderDump, 'Fixed', 'Trainer',
    'Translation', 'Pirate', 'Cracked', 'Modified', 'Hack');

type

  { TfmLEmuTKIcnSoftList }

  TfmLEmuTKIcnSoftList = class(TfmEmutecaSoftList)

    procedure VSTDrawText(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellText: string; const CellRect: TRect;
      var DefaultDraw: boolean);

  private
    FDumpIconList: cCHXImageList;
    FEmuteca: cEmuteca;
    FGUIConfig: cGUIConfig;
    FSoftIconList: cCHXImageList;
    FZoneIconMap: cCHXImageMap;
    procedure SetDumpIconList(AValue: cCHXImageList);
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetGUIConfig(AValue: cGUIConfig);
    procedure SetSoftIconList(AValue: cCHXImageList);
    procedure SetZoneIconMap(AValue: cCHXImageMap);

  public
    property GUIConfig: cGUIConfig read FGUIConfig write SetGUIConfig;
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    property SoftIconList: cCHXImageList
      read FSoftIconList write SetSoftIconList;
    //< Game, group, systems and emulatros icons
    property DumpIconList: cCHXImageList
      read FDumpIconList write SetDumpIconList;
    //< Icons of dump info.
    property ZoneIconMap: cCHXImageMap read FZoneIconMap write SetZoneIconMap;
    //< Icons for zones

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKIcnSoftList }

procedure TfmLEmuTKIcnSoftList.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: string; const CellRect: TRect; var DefaultDraw: boolean);
var
  pData: ^cEmutecaSoftware;
  IconRect: TRect;
  aIcon: TPicture;
  i: integer;
  TmpStr: string;
begin
  DefaultDraw := True;

  case Column of
    0: // System
    begin
      if (Node = nil) then
        Exit;
      if not assigned(SoftIconList) then
        Exit;
      pData := VST.GetNodeData(Node);
      if (pData^ = nil) then
        Exit;
      if not assigned(pData^.System) then
      Exit;

      DefaultDraw := False;

      // Icon space
      IconRect := CellRect;
      IconRect.Right := IconRect.Left + IconRect.Bottom - IconRect.Top;


        if pData^.System.Stats.IconIndex = -1 then
        begin
          if FileExistsUTF8(pData^.System.Icon) then
            pData^.System.Stats.IconIndex :=
              SoftIconList.AddImageFile(pData^.System.Icon)
          else
            pData^.System.Stats.IconIndex := 0;
        end;

        if (pData^.System.Stats.IconIndex < SoftIconList.Count) then
        begin
          aIcon := SoftIconList[pData^.System.Stats.IconIndex];
          TargetCanvas.StretchDraw(CorrectAspectRatio(IconRect, aIcon),
            aIcon.Graphic);
        end;

      // Don't draw text

      // Text space
      //IconRect := CellRect;
      //IconRect.Left := IconRect.Left + IconRect.Bottom -
      //  IconRect.Top + VST.TextMargin;

      //DrawText(TargetCanvas.Handle, PChar(CellText), -1, IconRect,
      //  DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE or
      //  DT_WORDBREAK or DT_END_ELLIPSIS or DT_EDITCONTROL);
    end;

    1: // Title
    begin
      if (Node = nil) then
        Exit;
      if not assigned(SoftIconList) then
        Exit;


      pData := VST.GetNodeData(Node);
      if (pData^ = nil) then
        Exit;

      DefaultDraw := False;

      // Icon space
      IconRect := CellRect;
      IconRect.Right := IconRect.Left + IconRect.Bottom - IconRect.Top;

      //  if pData^.Stats.IconIndex = -1 then
      //  begin

      // Usual logic:
      // If GroupIcon = SoftIcon then
      //   Use/Search icon of group
      // else
      //   Search soft icon
      //   if not found then
      //      Use/Search icon of group

      // Used logic:
      // Search icon of group
      // If GroupIcon = SoftIcon then
      //   Use icon of group
      // else
      //   Search soft icon
      //   if not found then
      //      Use icon of group

      //  if pData^.Info.Group.Stats.IconIndex = -1 then
      //  begin // Searching group icon
      //    TmpStr := Emuteca.SearchFirstGroupFile(pData^.Info.System.IconFolder,
      //      pData^.Info.Group, GUIConfig.ImageExtensions);
      //    if TmpStr = '' then
      //      pData^.Info.Group.Stats.IconIndex := 0
      //    else
      //      pData^.Info.Group.Stats.IconIndex := SoftIconList.AddImageFile(TmpStr);
      //  end;

      //  // Dirty same file test
      //  if RemoveFromBrackets(ExtractFileNameOnly(pData^.Info.Group.ID)) =
      //    RemoveFromBrackets(ExtractFileNameOnly(pData^.FileName)) then
      //  begin
      //    pData^.Stats.IconIndex := pData^.Info.Group.Stats.IconIndex;
      //  end
      //  else
      //  begin
      //    TmpStr := Emuteca.SearchFirstSoftFile(pData^.Info.System.IconFolder,
      //      pData^, GUIConfig.ImageExtensions, False);
      //    if TmpStr = '' then
      //      pData^.Stats.IconIndex := pData^.Info.Group.Stats.IconIndex
      //    else
      //      pData^.Stats.IconIndex := SoftIconList.AddImageFile(TmpStr);
      //  end;
      //end;

      //if (pData^.Stats.IconIndex < SoftIconList.Count) then
      //begin
      //  aIcon := SoftIconList[pData^.Stats.IconIndex];
      //  TargetCanvas.StretchDraw(CorrectAspectRatio(IconRect, aIcon),
      //    aIcon.Graphic);
      //end;

      // Text space
      IconRect := CellRect;
      IconRect.Left := IconRect.Left + IconRect.Bottom -
        IconRect.Top + VST.TextMargin;

      DrawText(TargetCanvas.Handle, PChar(CellText), -1, IconRect,
        DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE or
        DT_WORDBREAK or DT_END_ELLIPSIS or DT_EDITCONTROL);
    end;

    2: // Version
    begin
      if not assigned(ZoneIconMap) then
        Exit;

      if (Node = nil) then
        Exit;
      pData := VST.GetNodeData(Node);
      if (pData^ = nil) then
        Exit;

      DefaultDraw := False;

      // Icon space
      IconRect := CellRect;
      IconRect.Right := IconRect.Left + IconRect.Bottom - IconRect.Top;

      if not ZoneIconMap.TryGetData(pData^.Zone, aIcon) then
        ZoneIconMap.TryGetData('', aIcon);
      if assigned(aIcon) then
        TargetCanvas.StretchDraw(CorrectAspectRatio(IconRect, aIcon),
          aIcon.Graphic);

      // Text space
      IconRect := CellRect;
      IconRect.Left := IconRect.Left + IconRect.Bottom -
        IconRect.Top + VST.TextMargin;

      DrawText(TargetCanvas.Handle, PChar(pData^.Version), -1, IconRect,
        DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE or
        DT_WORDBREAK or DT_END_ELLIPSIS or DT_EDITCONTROL);
    end;

    5: // Flags
    begin
      if not assigned(DumpIconList) then
        Exit;

      if (Node = nil) then
        Exit;
      pData := VST.GetNodeData(Node);
      if (pData^ = nil) then
        Exit;

      DefaultDraw := False;

      IconRect := CellRect;
      IconRect.Right := IconRect.Left + IconRect.Bottom - IconRect.Top;

      // DumpStatus (0-5)
      TargetCanvas.StretchDraw(CorrectAspectRatio(IconRect,
        DumpIconList[Ord(pData^.DumpStatus)]),
        DumpIconList[Ord(pData^.DumpStatus)].Graphic);

      // Others
      for i := 6 to High(LazEmuTKIconFiles) do
      begin
        IconRect.Left := IconRect.Left + IconRect.Bottom - IconRect.Top;
        IconRect.Right := IconRect.Right + IconRect.Bottom - IconRect.Top;

        case i of
          6: // Fixed
            TmpStr := pData^.Fixed;
          7: // Trainer
            TmpStr := pData^.Trainer;
          8: // Translation;
            TmpStr := pData^.Translation;
          9: // Pirate
            TmpStr := pData^.Pirate;
          10: // Cracked
            TmpStr := pData^.Cracked;
          11: // Modified
            TmpStr := pData^.Modified;
          12: // Hack
            TmpStr := pData^.Hack;
          else
            TmpStr := '';
        end;

        // Draw icon
        if (TmpStr <> '') then
        begin
          TargetCanvas.StretchDraw(CorrectAspectRatio(IconRect,
            DumpIconList[i]), DumpIconList[i].Graphic);

          // Some magic
          case i of
            8: // Translation;
            begin
              if (TmpStr[1] = '+') then
                TmpStr := Trim(UTF8Copy(TmpStr, 2, 3))
              else  // GoodXXX or "-" TOSEC
                TmpStr := Trim(UTF8Copy(TmpStr, 1, 3));

              //Drawing text over icon
              DrawText(TargetCanvas.Handle, PChar(TmpStr), -1, IconRect,
                DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE or
                DT_EDITCONTROL or DT_CENTER);
            end;
            else
              ;
          end;
        end;
      end;
    end;
    else
      DefaultDraw := True;
  end;
end;

procedure TfmLEmuTKIcnSoftList.SetSoftIconList(AValue: cCHXImageList);
begin
  if FSoftIconList = AValue then
    Exit;
  FSoftIconList := AValue;
end;

procedure TfmLEmuTKIcnSoftList.SetZoneIconMap(AValue: cCHXImageMap);
begin
  if FZoneIconMap = AValue then
    Exit;
  FZoneIconMap := AValue;
end;

constructor TfmLEmuTKIcnSoftList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  // Set Width of icon columns
  // System
  vst.Header.Columns[0].Width :=
    vst.DefaultNodeHeight + vst.Header.Columns[0].Spacing * 2;
  // DumpStatus
  vst.Header.Columns[5].Width :=
    vst.DefaultNodeHeight * 8 + vst.Header.Columns[5].Spacing * 2;
end;

destructor TfmLEmuTKIcnSoftList.Destroy;
begin
  inherited Destroy;
end;

procedure TfmLEmuTKIcnSoftList.SetDumpIconList(AValue: cCHXImageList);
begin
  if FDumpIconList = AValue then
    Exit;
  FDumpIconList := AValue;
end;

procedure TfmLEmuTKIcnSoftList.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;
end;

procedure TfmLEmuTKIcnSoftList.SetGUIConfig(AValue: cGUIConfig);
begin
  if FGUIConfig = AValue then
    Exit;
  FGUIConfig := AValue;

  ReadActionsIcons(GUIConfig.GUIIcnFile, Self.Name, ilSoftList, alSoftList);
end;

end.
