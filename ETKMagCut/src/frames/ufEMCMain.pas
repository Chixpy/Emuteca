unit ufEMCMain;

{< ETK Magazine Cutter main frame.

  This file is part of ETK Magazine.

  Copyright (C) 2022 Chixpy

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, LazUTF8, LazFileUtils, BGRABitmap, BGRABitmapTypes,
  // CHX frames
  ufCHXFrame, ufCHXFileList, ufCHXBGRAImgViewerEx,
  // EMC classes
  ucEMCConfig,
  // ETK Magazine Cutter frames
  ufEMCImagePropEditor;

type

  { TfmEMCMain }

  TfmEMCMain = class(TfmCHXFrame)
    bCutSelection: TButton;
    bReload: TButton;
    bReplaceFile: TButton;
    gbxFileList: TGroupBox;
    gbxImage: TGroupBox;
    gbxProperties: TGroupBox;
    pButtons: TPanel;
    pEmpty: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure bCutSelectionClick(Sender: TObject);
    procedure bReloadClick(Sender: TObject);
    procedure bReplaceFileClick(Sender: TObject);
  private
    FCurrentImage: TBGRABitmap;
    FModifiedImage: Boolean;
    FfmFileList: TfmCHXFileList;
    FfmImage: TfmCHXBGRAImgViewerEx;
    FfmPropEditor: TfmEMCImagePropEditor;
    FEMCConfig: cEMCConfig;
    procedure SetModifiedImage(AValue: Boolean);
    procedure SetEMCConfig(AValue: cEMCConfig);

  protected
    property ModifiedImage: Boolean read FModifiedImage write SetModifiedImage;

    property fmFileList: TfmCHXFileList read FfmFileList;
    property fmPropEditor: TfmEMCImagePropEditor read FfmPropEditor;
    property fmImage: TfmCHXBGRAImgViewerEx read FfmImage;

    procedure DoFileSelect(aFile: string);
    procedure DoFileSave(aFilename: string; Resize2048: boolean);
    procedure DoFileDelete;
    procedure DoJoinNextFile;
    procedure DoRectSelect(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; aRect: TRect);
    procedure DoRectChange(aRect: TRect);
    procedure DoPMEnabled(PMEnabled: boolean);
    procedure DoPMClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);

  public
    CurrentRect: TRect;

    property CurrentImage: TBGRABitmap read FCurrentImage;

    property EMCConfig: cEMCConfig read FEMCConfig write SetEMCConfig;

    procedure SaveEMCConfig;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEMCMain }

procedure TfmEMCMain.bReloadClick(Sender: TObject);
begin
  DoFileSelect(fmPropEditor.ImageFile);
end;

procedure TfmEMCMain.bReplaceFileClick(Sender: TObject);
begin
  if fmPropEditor.ImageFile = '' then Exit;

  CurrentImage.SaveToFileUTF8(fmPropEditor.ImageFile);
  ModifiedImage := False;
end;

procedure TfmEMCMain.SetEMCConfig(AValue: cEMCConfig);
begin
  if FEMCConfig = AValue then Exit;
  FEMCConfig := AValue;

  fmPropEditor.EMCConfig := EMCConfig;
end;

procedure TfmEMCMain.SetModifiedImage(AValue: Boolean);
begin
  if FModifiedImage = AValue then Exit;
  FModifiedImage := AValue;

  bReplaceFile.Enabled := AValue;
end;

procedure TfmEMCMain.bCutSelectionClick(Sender: TObject);
var
  FullImageRect: TRect;
begin
  if CurrentRect.IsEmpty then Exit;

  fmImage.ActualImage := nil;

  FullImageRect := TRect.Create(0, 0, CurrentImage.Width,
    CurrentImage.Height);

  // Cropping CurrentRect to image coordinates, It can be outbounds and GetPart
  //   will wrap arround it
  CurrentRect.Intersect(FullImageRect);

  // Don't cut it if full image is selected
  if CurrentRect = FullImageRect then Exit;

  BGRAReplace(FCurrentImage, CurrentImage.GetPart(CurrentRect));
  CurrentRect := CurrentRect.Empty;

  fmImage.ActualImage := CurrentImage;

  fmPropEditor.SetRect(CurrentRect);

  ModifiedImage := True;
end;

procedure TfmEMCMain.DoFileSelect(aFile: string);
begin
  fmImage.ActualImage := nil;
  FreeAndNil(FCurrentImage);
  CurrentRect := CurrentRect.Empty;

  if FileExistsUTF8(aFile) then
  begin
    fmPropEditor.ImageFile := aFile;
    FCurrentImage := TBGRABitmap.Create(aFile, True);
  end
  else
  begin
    fmPropEditor.ImageFile := '';
  end;

  fmImage.ActualImage := CurrentImage;

  ModifiedImage := False;
end;

procedure TfmEMCMain.DoFileSave(aFilename: string; Resize2048: boolean);
var
  TempImg: TBGRABitmap;
  aSize: integer;
  aFactor: single;

begin
  if not Assigned(FCurrentImage) then Exit;

  if CurrentRect.IsEmpty then
  begin
    TempImg := CurrentImage.Duplicate(True);
  end
  else
  begin
    CurrentRect.Intersect(TRect.Create(0, 0, CurrentImage.Width,
      CurrentImage.Height));
    TempImg := CurrentImage.GetPart(CurrentRect);
  end;

  if Resize2048 then
  begin
    if TempImg.Width < TempImg.Height then
      aSize := TempImg.Height
    else
      aSize := TempImg.Width;

    if aSize > 2048 then
    begin
      aFactor := aSize / 2048;

      TempImg.ResampleFilter := rfBestQuality;
      BGRAReplace(TempImg, TempImg.Resample(trunc(TempImg.Width / aFactor),
        trunc(TempImg.Height / aFactor), rmFineResample));
    end;
  end;

  ForceDirectoriesUTF8(ExtractFilePath(aFilename));
  TempImg.SaveToFileUTF8(aFilename);

  FreeAndNil(TempImg);
end;

procedure TfmEMCMain.DoFileDelete;
begin
  ModifiedImage := False;

  DeleteFileUTF8(fmPropEditor.ImageFile);
  DoFileSelect('');
  fmFileList.actRemoveItemExecute(nil);
end;

procedure TfmEMCMain.DoJoinNextFile;
var
  NextFileName: string;
  TempImg, NextImage: TBGRABitmap;
  aW, aH: integer;
begin
  if not Assigned(FCurrentImage) then Exit;

  NextFileName := fmFileList.NextFile;

  if (NextFileName = '') or (not FileExistsUTF8(NextFileName)) then Exit;

  fmImage.ActualImage := nil;

  NextImage := TBGRABitmap.Create(NextFileName, True);

  // New image width and height
  aW := CurrentImage.Width + NextImage.Width;

  if CurrentImage.Height > NextImage.Height then
    aH := CurrentImage.Height
  else
    aH := NextImage.Height;

  TempImg := TBGRABitmap.Create(aW, aH, BGRA(0,0,0,0));

  TempImg.PutImage(0, 0, CurrentImage, dmDrawWithTransparency);
  TempImg.PutImage(CurrentImage.Width, 0, NextImage, dmDrawWithTransparency);

  BGRAReplace(FCurrentImage, TempImg);

  FreeAndNil(NextImage);

  fmImage.ActualImage := CurrentImage;

  ModifiedImage := True;
end;

procedure TfmEMCMain.DoRectSelect(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; aRect: TRect);
begin
  if not Assigned(FCurrentImage) then Exit;
  CurrentRect := aRect;
  CurrentRect.NormalizeRect;
  CurrentRect.Intersect(TRect.Create(0, 0, CurrentImage.Width,
    CurrentImage.Height));
  fmPropEditor.SetRect(CurrentRect);
end;

procedure TfmEMCMain.DoRectChange(aRect: TRect);
begin
  if not Assigned(FCurrentImage) then Exit;
  CurrentRect := aRect;
  CurrentRect.NormalizeRect;
  CurrentRect.Intersect(TRect.Create(0, 0, CurrentImage.Width,
    CurrentImage.Height));
  fmImage.SelectionRect := CurrentRect;
  // Changing SelectionRect don't update the image
  fmImage.LoadFrameData;
end;

procedure TfmEMCMain.DoPMEnabled(PMEnabled: boolean);
begin
  if PMEnabled then
  begin
    fmImage.OnImgMouseDrag := nil;
    fmImage.MouseActionMode := maiMouseClick;
    fmImage.OnImgMouseUp := @DoPMClick;
  end
  else
  begin
    fmImage.OnImgMouseUp := nil;
    fmImage.MouseActionMode := maiMouseSelectRect;
    fmImage.OnImgMouseDrag := @DoRectSelect;
  end;
end;

procedure TfmEMCMain.DoPMClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  aBorder: integer;
begin
  if not Assigned(FCurrentImage) then Exit;

  case Button of
    mbLeft:
    begin
      if Shift = [] then
      begin

        // Dirty...
        aBorder := fmPropEditor.ePMBorder.Value;

        if CurrentRect.IsEmpty then
        begin
          CurrentRect.Create(X - aBorder, Y - aBorder, X +
            aBorder, Y + aBorder);
        end
        else
        begin
          if CurrentRect.Left > (X - aBorder) then
            CurrentRect.Left := X - aBorder;
          if CurrentRect.Top > (Y - aBorder) then
            CurrentRect.Top := Y - aBorder;
          if CurrentRect.Right < (X + aBorder) then
            CurrentRect.Right := X + aBorder;
          if CurrentRect.Bottom < (Y + aBorder) then
            CurrentRect.Bottom := Y + aBorder;
        end;

        CurrentRect.Intersect(TRect.Create(0, 0, CurrentImage.Width,
          CurrentImage.Height));

        // Updating frames
        fmPropEditor.SetRect(CurrentRect);
        fmImage.SelectionRect := CurrentRect;
        // Changing SelectionRect don't update the image
        fmImage.LoadFrameData;
      end;
    end;
    mbRight:
    begin
      if Shift = [] then
      begin
        // RClick removes current rect

        CurrentRect := CurrentRect.Empty;

        // Updating frames
        fmPropEditor.SetRect(CurrentRect);
        fmImage.SelectionRect := CurrentRect;
        // Changing SelectionRect don't update the image
        fmImage.LoadFrameData;
      end;
    end;
    mbMiddle: ;
    mbExtra1: ;
    mbExtra2: ;
  end;
end;

procedure TfmEMCMain.SaveEMCConfig;
begin
  fmPropEditor.SaveEMCConfig;
end;

constructor TfmEMCMain.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FfmFileList := TfmCHXFileList.Create(gbxFileList);
    fmFileList.FileMask := GraphicFileMask(TGraphic);
    fmFileList.OnFileSelect := @DoFileSelect;
    fmFileList.SelectNextOnRemove := True;
    fmFileList.Align := alClient;
    fmFileList.Parent := gbxFileList;

    FfmPropEditor := TfmEMCImagePropEditor.Create(gbxProperties);
    fmPropEditor.OnSave := @DoFileSave;
    fmPropEditor.OnDelete := @DoFileDelete;
    fmPropEditor.OnChangeRect := @DoRectChange;
    fmPropEditor.OnPMChange := @DoPMEnabled;
    fmPropEditor.OnJoinNextFile := @DoJoinNextFile;
    fmPropEditor.Align := alClient;
    fmPropEditor.Parent := gbxProperties;

    FfmImage := TfmCHXBGRAImgViewerEx.Create(gbxImage);
    // Setting fmImage.MouseActionMode and fmImage.OnImgMouseDrag
    DoPMEnabled(fmPropEditor.chkPointMode.Checked);
    fmImage.AutoZoomOnLoad := True;
    // RClick is used for clear selection rest
    fmImage.PopUpMenuEnabled := False;
    fmImage.Align := alClient;
    fmImage.Parent := gbxImage;
  end;

begin
  inherited Create(TheOwner);

  CreateFrames;

  Enabled := True;
end;

destructor TfmEMCMain.Destroy;
begin
  FreeAndNil(FCurrentImage);
  inherited Destroy;
end;

end.
