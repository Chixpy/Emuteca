unit ufETKGUISoftMedia;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  // CHX frames
  ufCHXFrame,
  // Emuteca classes
  ucEmutecaGroup, ucEmutecaSoftware,
  // Emuteca GUI frames
  ufETKGUISoftImgPreview, ufETKGUISoftTxtPreview, ufETKGUISoftVideoPreview;

type

  { TfmETKGUISoftMedia }

  TfmETKGUISoftMedia = class(TfmCHXFrame)
    pImagePreview: TPanel;
    pTxtPreview: TPanel;
    pVideoPreview: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;

  private
    FGroup: cEmutecaGroup;
    FImageExt: TStrings;
    FMPlayerPath: string;
    FMusicExt: TStrings;
    FSHA1Folder: string;
    FSoftImgPreview: TfmETKGUISoftImgPreview;
    FSoftTxtPreview: TfmETKGUISoftTxtPreview;
    FSoftVideoPreview: TfmETKGUISoftVideoPreview;
    FSoftware: cEmutecaSoftware;
    FTextExt: TStrings;
    FVideoExt: TStrings;
    procedure SetGroup(AValue: cEmutecaGroup);
    procedure SetImageExt(AValue: TStrings);
    procedure SetMPlayerPath(const aMPlayerPath: string);
    procedure SetMusicExt(const aMusicExt: TStrings);
    procedure SetSHA1Folder(const AValue: string);
    procedure SetSoftware(AValue: cEmutecaSoftware);
    procedure SetTextExt(AValue: TStrings);
    procedure SetVideoExt(const aVideoExt: TStrings);

  protected
    property SoftImgPreview: TfmETKGUISoftImgPreview read FSoftImgPreview;
    property SoftTxtPreview: TfmETKGUISoftTxtPreview read FSoftTxtPreview;
    property SoftVideoPreview: TfmETKGUISoftVideoPreview read FSoftVideoPreview;

  public
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
    property Group: cEmutecaGroup read FGroup write SetGroup;

    property ImageExt: TStrings read FImageExt write SetImageExt;
    property TextExt: TStrings read FTextExt write SetTextExt;
    property MusicExt: TStrings read FMusicExt write SetMusicExt;
    property VideoExt: TStrings read FVideoExt write SetVideoExt;

    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    property MPlayerPath: string read FMPlayerPath write SetMPlayerPath;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.lfm}

{ TfmETKGUISoftMedia }

procedure TfmETKGUISoftMedia.SetGroup(AValue: cEmutecaGroup);
begin
  if FGroup = AValue then
    Exit;
  FGroup := AValue;

  SoftImgPreview.Group := Group;
  SoftTxtPreview.Group := Group;
  SoftVideoPreview.Group := Group;

  LoadFrameData;
end;

procedure TfmETKGUISoftMedia.SetImageExt(AValue: TStrings);
begin
  if FImageExt = AValue then Exit;
  FImageExt := AValue;

  SoftImgPreview.FileExt := ImageExt;
end;

procedure TfmETKGUISoftMedia.SetMPlayerPath(const aMPlayerPath: string);
begin
  if FMPlayerPath = aMPlayerPath then Exit;
  FMPlayerPath := aMPlayerPath;

  SoftVideoPreview.MPlayerPath := FMPlayerPath;
end;

procedure TfmETKGUISoftMedia.SetMusicExt(const aMusicExt: TStrings);
begin
  if FMusicExt = aMusicExt then Exit;
  FMusicExt := aMusicExt;

  // SoftMusicPreview.FileExt := MusicExt;
end;

procedure TfmETKGUISoftMedia.SetSHA1Folder(const AValue: string);
begin
  if FSHA1Folder = AValue then Exit;
  FSHA1Folder := AValue;

  SoftImgPreview.SHA1Folder := SHA1Folder;
end;

procedure TfmETKGUISoftMedia.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;
  FSoftware := AValue;

  SoftImgPreview.Software := Software;
  SoftTxtPreview.Software := Software;
  SoftVideoPreview.Software := Software;

  LoadFrameData;
end;

procedure TfmETKGUISoftMedia.SetTextExt(AValue: TStrings);
begin
  if FTextExt = AValue then Exit;
  FTextExt := AValue;

  SoftTxtPreview.FileExt := TextExt;
end;

procedure TfmETKGUISoftMedia.SetVideoExt(const aVideoExt: TStrings);
begin
  if FVideoExt = aVideoExt then Exit;
  FVideoExt := aVideoExt;

  SoftVideoPreview.FileExt := VideoExt;
end;

constructor TfmETKGUISoftMedia.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FSoftImgPreview := TfmETKGUISoftImgPreview.Create(pImagePreview);
    SoftImgPreview.Align := alClient;
    SoftImgPreview.Parent := pImagePreview;

    FSoftTxtPreview := TfmETKGUISoftTxtPreview.Create(pTxtPreview);
    SoftTxtPreview.Align := alClient;
    SoftTxtPreview.Parent := pTxtPreview;

    FSoftVideoPreview := TfmETKGUISoftVideoPreview.Create(pVideoPreview);
    SoftVideoPreview.Align := alClient;
    SoftVideoPreview.Parent := pVideoPreview;
  end;

begin
  inherited Create(TheOwner);

  Enabled := True; // This frame can be enabled while empty to move splitter

  CreateFrames;
end;

destructor TfmETKGUISoftMedia.Destroy;
begin
  inherited Destroy;
end;

end.
