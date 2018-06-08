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
  ufETKGUISoftImgPreview, ufETKGUISoftTxtPreview;

type

  { TfmETKGUISoftMedia }

  TfmETKGUISoftMedia = class(TfmCHXFrame)
    pSoftImagPreview: TPanel;
    pSoftTxtPreview: TPanel;
    Splitter1: TSplitter;

  private
    FGroup: cEmutecaGroup;
    FImageExt: TStrings;
    FSHA1Folder: string;
    FSoftImgPreview: TfmETKGUISoftImgPreview;
    FSoftTxtPreview: TfmETKGUISoftTxtPreview;
    FSoftware: cEmutecaSoftware;
    FTextExt: TStrings;
    procedure SetGroup(AValue: cEmutecaGroup);
    procedure SetImageExt(AValue: TStrings);
    procedure SetSHA1Folder(AValue: string);
    procedure SetSoftware(AValue: cEmutecaSoftware);
    procedure SetTextExt(AValue: TStrings);

  protected
    property SoftImgPreview: TfmETKGUISoftImgPreview read FSoftImgPreview;
    property SoftTxtPreview: TfmETKGUISoftTxtPreview read FSoftTxtPreview;

  public
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
    property Group: cEmutecaGroup read FGroup write SetGroup;

    property ImageExt: TStrings read FImageExt write SetImageExt;
    property TextExt: TStrings read FTextExt write SetTextExt;

    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

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

  LoadFrameData;
end;

procedure TfmETKGUISoftMedia.SetImageExt(AValue: TStrings);
begin
  if FImageExt = AValue then Exit;
  FImageExt := AValue;

  SoftImgPreview.FileExt := ImageExt;
end;

procedure TfmETKGUISoftMedia.SetSHA1Folder(AValue: string);
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

  LoadFrameData;
end;

procedure TfmETKGUISoftMedia.SetTextExt(AValue: TStrings);
begin
  if FTextExt = AValue then Exit;
  FTextExt := AValue;

  SoftTxtPreview.FileExt := TextExt;
end;

constructor TfmETKGUISoftMedia.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FSoftImgPreview := TfmETKGUISoftImgPreview.Create(pSoftImagPreview);
    SoftImgPreview.Align := alClient;
    SoftImgPreview.Parent := pSoftImagPreview;

    FSoftTxtPreview := TfmETKGUISoftTxtPreview.Create(pSoftTxtPreview);
    SoftTxtPreview.Align := alClient;
    SoftTxtPreview.Parent := pSoftTxtPreview;
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
