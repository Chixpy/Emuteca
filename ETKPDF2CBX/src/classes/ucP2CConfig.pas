unit ucP2CConfig;
{< cP2CConfig class unit.

  This file is part of ETK PDF2CBX.

  Copyright (C) 2022 Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  // CHX units
  uCHXStrUtils,
  // CHX abstracts
  uaCHXConfig;

const
  P2CIniExeSect = 'Executables';
  P2CIniPDFtoPNGKey = 'PDFtoPNG';
  P2CIniPDFImagesKey = 'PDFImages';
  P2CIni7zKey = '7z';
  P2CIniImageEditorKey = 'ImageEditor';
  P2CIniImageEditorParamsKey = 'ImageEditorParams';
  P2CIniOptSect = 'Options';
  P2CIniLastFolderKey = 'LastFolder';

type

  { cP2CConfig }

  cP2CConfig = class(caCHXConfig)

  private
    FImgEditorExe: string;
    FImgEditorParams: string;
    FLastFolder: string;
    FPDFImagesExe: string;
    FPDFtoPNGExe: string;
    FSevenZipExe: string;
    procedure SetImgEditorExe(AValue: string);
    procedure SetImgEditorParams(AValue: string);
    procedure SetLastFolder(AValue: string);
    procedure SetPDFImagesExe(AValue: string);
    procedure SetPDFtoPNGExe(AValue: string);
    procedure SetSevenZipExe(AValue: string);

  public
    property PDFtoPNGExe: string read FPDFtoPNGExe write SetPDFtoPNGExe;
    property PDFImagesExe: string read FPDFImagesExe write SetPDFImagesExe;
    property SevenZipExe: string read FSevenZipExe write SetSevenZipExe;
    property ImgEditorExe: string read FImgEditorExe write SetImgEditorExe;
    property ImgEditorParams: string read FImgEditorParams
      write SetImgEditorParams;

    property LastFolder: string read FLastFolder write SetLastFolder;

    procedure LoadFromIni(aIniFile: TMemIniFile); override;
    procedure ResetDefaultConfig; override;
    {< Sets config properties to default values. }

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveToIni(aIniFile: TMemIniFile); override;
  end;

implementation

{ cP2CConfig }

procedure cP2CConfig.SetPDFtoPNGExe(AValue: string);
begin
  FPDFtoPNGExe := SetAsFile(AValue);
end;

procedure cP2CConfig.SetImgEditorExe(AValue: string);
begin
  FImgEditorExe := SetAsFile(AValue);
end;

procedure cP2CConfig.SetImgEditorParams(AValue: string);
begin
  if FImgEditorParams = AValue then Exit;
  FImgEditorParams := AValue;
end;

procedure cP2CConfig.SetLastFolder(AValue: string);
begin
  FLastFolder := SetAsFolder(AValue);
end;

procedure cP2CConfig.SetPDFImagesExe(AValue: string);
begin
  FPDFImagesExe := SetAsFile(AValue);
end;

procedure cP2CConfig.SetSevenZipExe(AValue: string);
begin
  FSevenZipExe := SetAsFile(AValue);
end;

procedure cP2CConfig.LoadFromIni(aIniFile: TMemIniFile);
begin
  PDFtoPNGExe := aIniFile.ReadString(P2CIniExeSect, P2CIniPDFtoPNGKey, PDFtoPNGExe);
  PDFImagesExe := aIniFile.ReadString(P2CIniExeSect, P2CIniPDFImagesKey, PDFImagesExe);
  SevenZipExe := aIniFile.ReadString(P2CIniExeSect, P2CIni7zKey, SevenZipExe);
  ImgEditorExe := aIniFile.ReadString(P2CIniExeSect, P2CIniImageEditorKey,
    ImgEditorExe);
  ImgEditorParams := aIniFile.ReadString(P2CIniExeSect, P2CIniImageEditorParamsKey, ImgEditorParams);

  LastFolder := aIniFile.ReadString(P2CIniOptSect, P2CIniLastFolderKey, LastFolder);
end;

procedure cP2CConfig.ResetDefaultConfig;
begin
  PDFtoPNGExe := 'Tools\pdftopng.exe';
  PDFImagesExe := 'Tools\pdfimages.exe';
  SevenZipExe := 'Tools\7zip\7z.exe';
  ImgEditorExe := '';
  ImgEditorParams := '"%FILE%"';

  LastFolder := '';
end;

procedure cP2CConfig.SaveToIni(aIniFile: TMemIniFile);
begin
  aIniFile.WriteString(P2CIniExeSect, P2CIniPDFtoPNGKey, PDFtoPNGExe);
  aIniFile.WriteString(P2CIniExeSect, P2CIniPDFImagesKey, PDFImagesExe);
  aIniFile.WriteString(P2CIniExeSect, P2CIni7zKey, SevenZipExe);
  aIniFile.WriteString(P2CIniExeSect, P2CIniImageEditorKey, ImgEditorExe);
  aIniFile.WriteString(P2CIniExeSect, P2CIniImageEditorParamsKey, ImgEditorParams);

  aIniFile.WriteString(P2CIniOptSect, P2CIniLastFolderKey, LastFolder);

  aIniFile.UpdateFile;
end;

constructor cP2CConfig.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor cP2CConfig.Destroy;
begin
  inherited Destroy;
end;

end.
{
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
