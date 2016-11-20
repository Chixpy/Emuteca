unit ufLEmuTKIcnSoftList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  VirtualTrees, LCLIntf, LCLType, LazUTF8,
  ucCHXImageList, uCHXImageUtils,
  ucEmutecaSoftware, ucEmutecaGroup,
  ufEmutecaSoftList;

const
  LazEmuTKIconFiles: array [0..12] of string =
    (krsedsVerified, krsedsGood, krsedsAlternate, krsedsOverDump,
    krsedsBadDump, krsedsUnderDump, 'Fixed', 'Trainer',
    'Translation', 'Pirate',
    'Cracked', 'Modified', 'Hack');

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
    FZoneIconMap: cCHXImageMap;
    procedure SetDumpIconList(AValue: cCHXImageList);
    procedure SetSoftIconList(AValue: cCHXImageList);
    procedure SetZoneIconMap(AValue: cCHXImageMap);
    { private declarations }
  public
    { public declarations }

    property SoftIconList: cCHXImageList
      read FSoftIconList write SetSoftIconList;
    //< Game icons (exclusive ones).
    property DumpIconList: cCHXImageList
      read FDumpIconList write SetDumpIconList;
    {< Icons of dump info. }
    property ZoneIconMap: cCHXImageMap read FZoneIconMap write SetZoneIconMap;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
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
  aIcon: TPicture;
  i: integer;
  TmpStr: string;
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
        // TODO: Search Icon, extract it, add to list, add to caché


        // Icon not found, search parent one
        if not assigned(Data^.Group) then
        begin
          // Search parent add to cache
         // if Data^.Group = -1 then
          //  Data^.Group.Stats.IconIndex := 0; // Assign default icon to parent        end;
        end;

        if assigned(Data^.Group) then
            Data^.Stats.IconIndex := Data^.Group.Stats.IconIndex
        else
         Data^.Stats.IconIndex := 0;
      end;

      if (Data^.Stats.IconIndex > -1) and
        (Data^.Stats.IconIndex < SoftIconList.Count) then
      begin
        aIcon := SoftIconList[Data^.Stats.IconIndex];
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

    2: // Version
    begin
      if not assigned(ZoneIconMap) then
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

      if not ZoneIconMap.TryGetData(Data^.Zone, aIcon) then
        ZoneIconMap.TryGetData('', aIcon);
      if assigned(aIcon) then
        TargetCanvas.StretchDraw(CorrectAspectRatio(IconRect, aIcon),
          aIcon.Graphic);

      // Text space
      IconRect := CellRect;
      IconRect.Left := IconRect.Left + IconRect.Bottom -
        IconRect.Top + VST.TextMargin;

      DrawText(TargetCanvas.Handle, PChar(Data^.Version), -1, IconRect,
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

      IconRect := CellRect;
      IconRect.Right := IconRect.Left + IconRect.Bottom - IconRect.Top;

      // DumpStatus (0-5)
      TargetCanvas.StretchDraw(CorrectAspectRatio(IconRect,
        DumpIconList[Ord(Data^.DumpStatus)]),
        DumpIconList[Ord(Data^.DumpStatus)].Graphic);

      // Others
      for i := 6 to High(LazEmuTKIconFiles) do
      begin
        IconRect.Left := IconRect.Left + IconRect.Bottom - IconRect.Top;
        IconRect.Right := IconRect.Right + IconRect.Bottom - IconRect.Top;

        case i of
          6: // Fixed
            TmpStr := Data^.Fixed;
          7: // Trainer
            TmpStr := Data^.Trainer;
          8: // Translation;
            TmpStr := Data^.Translation;
          9: // Pirate
            TmpStr := Data^.Pirate;
          10: // Cracked
            TmpStr := Data^.Cracked;
          11: // Modified
            TmpStr := Data^.Modified;
          12: // Hack
            TmpStr := Data^.Hack;
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

procedure TfmEmutecaIcnSoftList.SetSoftIconList(AValue: cCHXImageList);
begin
  if FSoftIconList = AValue then
    Exit;
  FSoftIconList := AValue;
end;

procedure TfmEmutecaIcnSoftList.SetZoneIconMap(AValue: cCHXImageMap);
begin
  if FZoneIconMap = AValue then
    Exit;
  FZoneIconMap := AValue;
end;

constructor TfmEmutecaIcnSoftList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  // Set Width of tags column
  vst.Header.Columns[5].Width :=
    vst.DefaultNodeHeight * 8 + vst.Header.Columns[5].Spacing * 2;
end;

destructor TfmEmutecaIcnSoftList.Destroy;
begin
  inherited Destroy;
end;

procedure TfmEmutecaIcnSoftList.SetDumpIconList(AValue: cCHXImageList);
begin
  if FDumpIconList = AValue then
    Exit;
  FDumpIconList := AValue;
end;

end.
