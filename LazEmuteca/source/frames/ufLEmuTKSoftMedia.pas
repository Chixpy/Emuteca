unit ufLEmuTKSoftMedia;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ufCHXFrame,
  ucEmutecaGroup, ucEmutecaSoftware,
  ufLEmuTKSoftImgPreview, ufLEmuTKSoftTxtPreview;

type

  { TfmLEmuTKSoftMedia }

  TfmLEmuTKSoftMedia = class(TfmCHXFrame)
    pSoftImagPreview: TPanel;
    pSoftTxtPreview: TPanel;
    Splitter1: TSplitter;

  private
    FGroup: cEmutecaGroup;
    FImageExt: TStrings;
    FSHA1Folder: string;
    FSoftImgPreview: TfmLEmuTKSoftImgPreview;
    FSoftTxtPreview: TfmLEmuTKSoftTxtPreview;
    FSoftware: cEmutecaSoftware;
    FTextExt: TStrings;
    procedure SetGroup(AValue: cEmutecaGroup);
    procedure SetImageExt(AValue: TStrings);
    procedure SetSHA1Folder(AValue: string);
    procedure SetSoftware(AValue: cEmutecaSoftware);
    procedure SetTextExt(AValue: TStrings);

  protected
    property SoftImgPreview: TfmLEmuTKSoftImgPreview read FSoftImgPreview;
    property SoftTxtPreview: TfmLEmuTKSoftTxtPreview read FSoftTxtPreview;

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

{ TfmLEmuTKSoftMedia }

procedure TfmLEmuTKSoftMedia.SetGroup(AValue: cEmutecaGroup);
begin
  if FGroup = AValue then
    Exit;
  FGroup := AValue;

  SoftImgPreview.Group := Group;
  SoftTxtPreview.Group := Group;

  LoadFrameData;
end;

procedure TfmLEmuTKSoftMedia.SetImageExt(AValue: TStrings);
begin
  if FImageExt = AValue then Exit;
  FImageExt := AValue;

  SoftImgPreview.FileExt := ImageExt;
end;

procedure TfmLEmuTKSoftMedia.SetSHA1Folder(AValue: string);
begin
  if FSHA1Folder = AValue then Exit;
  FSHA1Folder := AValue;

  SoftImgPreview.SHA1Folder := SHA1Folder;
end;

procedure TfmLEmuTKSoftMedia.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;
  FSoftware := AValue;

  SoftImgPreview.Software := Software;
  SoftTxtPreview.Software := Software;

  LoadFrameData;
end;

procedure TfmLEmuTKSoftMedia.SetTextExt(AValue: TStrings);
begin
  if FTextExt = AValue then Exit;
  FTextExt := AValue;

  SoftTxtPreview.FileExt := TextExt;
end;

constructor TfmLEmuTKSoftMedia.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FSoftImgPreview := TfmLEmuTKSoftImgPreview.Create(pSoftImagPreview);
    SoftImgPreview.Align := alClient;
    SoftImgPreview.Parent := pSoftImagPreview;

    FSoftTxtPreview := TfmLEmuTKSoftTxtPreview.Create(pSoftTxtPreview);
    SoftTxtPreview.Align := alClient;
    SoftTxtPreview.Parent := pSoftTxtPreview;
  end;

begin
  inherited Create(TheOwner);

  Enabled := True; // This frame can be enabled while empty to move splitter

  CreateFrames;
end;

destructor TfmLEmuTKSoftMedia.Destroy;
begin
  inherited Destroy;
end;

end.
