{ This file is part of Emuteca

  Copyright (C) 2006-2017 Chixpy

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

unit uaEmutecaCustomSystem;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LazFileUtils, LazUTF8,
  uCHXStrUtils,
  uaCHXStorable,
  ucCHXImageList,
  uEmutecaCommon,
  ucEmutecaPlayingStats;

type
  { caEmutecaCustomSystem }

  caEmutecaCustomSystem = class(caCHXStorableIni)
  private
    FBackImage: string;
    FBaseFolder: string;
    FEnabled: boolean;
    FExtensions: TStringList;
    FExtractAll: boolean;
    FFileName: string;
    FSoftExportKey: TEmutecaSoftExportKey;
    FIcon: string;
    FIconFolder: string;
    FID: string;
    FImage: string;
    FImageCaptions: TStringList;
    FImageFolders: TStringList;
    FInfoText: string;
    FMainEmulator: string;
    FMusicCaptions: TStringList;
    FMusicFolders: TStringList;
    FOtherEmulators: TStringList;
    FStats: cEmutecaPlayingStats;
    FTempFolder: string;
    FWorkingFolder: string;
    FTextCaptions: TStringList;
    FTextFolders: TStringList;
    FTitle: string;
    FVideoCaptions: TStringList;
    FVideoFolders: TStringList;
    procedure SetBackImage(AValue: string);
    procedure SetBaseFolder(AValue: string);
    procedure SetEnabled(AValue: boolean);
    procedure SetExtractAll(AValue: boolean);
    procedure SetFileName(AValue: string);
    procedure SetSoftExportKey(AValue: TEmutecaSoftExportKey);
    procedure SetIcon(AValue: string);
    procedure SetIconFolder(AValue: string);
    procedure SetID(AValue: string);
    procedure SetImage(AValue: string);
    procedure SetInfoText(AValue: string);
    procedure SetMainEmulator(AValue: string);
    procedure SetTempFolder(AValue: string);
    procedure SetWorkingFolder(AValue: string);
    procedure SetTitle(AValue: string);

  protected
    procedure FixFolderListData(FolderList, CaptionList: TStrings);
    {< Try to fix some incosistences in folders and its captions.

    Who knows if somebody edited the .ini file by hand...
    }

  public
    property TempFolder: string read FTempFolder write SetTempFolder;
    {< System temp folder for decompressing media
    }

    function MatchID(aID: string): boolean;
    function CompareID(aID: string): integer;

    procedure LoadFromIni(aIniFile: TIniFile); override;
    procedure SaveToIni(aIniFile: TMemIniFile; const ExportMode: boolean);
      override;

    procedure CacheIcon(aImagList: cCHXImageList);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published

    // Basic Info
    // ----------
    property ID: string read FID write SetID;
    {< ID of the system. }

    property Title: string read FTitle write SetTitle;
    {< Visible name (Usually "%Company%: %Model% %(info)%"}

    property FileName: string read FFileName write SetFileName;
    {< Name used for files or folders }

    property Enabled: boolean read FEnabled write SetEnabled;
    {< Is the system visible }

    property ExtractAll: boolean read FExtractAll write SetExtractAll;
    {< Must all files be extracted from compressed archives? }

    property BaseFolder: string read FBaseFolder write SetBaseFolder;
    {< System base folder

       Used by default to store some data if the file isn't defined
         (System image or text).
    }

    property WorkingFolder: string read FWorkingFolder write SetWorkingFolder;
    {< Temp folder for decompress Software, it's recommended leave it empty
         so Emuteca will use OS Temp folder.

       Some cases of use it:
       @unorderedList(
         @item(File must extracted to a specific folder. So emulator
           can recognize it.)
         @item(Emulator can't run from command line and you must browse to the
           file with the emulator... :-@ )
         @item(Lazy testing... >_<U )
       )
     }

    // Emulator related
    // ----------------
    property MainEmulator: string read FMainEmulator write SetMainEmulator;
    {< Main emulator ID. }
    property OtherEmulators: TStringList read FOtherEmulators;
    {< Ids of other emulators for the system. }

    // System Images
    // -------------
    property Icon: string read FIcon write SetIcon;
    {< Path to the icon of the system. }
    property Image: string read FImage write SetImage;
    {< Path to image of the system. }
    property BackImage: string read FBackImage write SetBackImage;
    {< Image used for as background. }

    // Soft image dirs
    // ---------------
    property IconFolder: string read FIconFolder write SetIconFolder;
    {< Folder for the icons of the games. }
    property ImageFolders: TStringList read FImageFolders;
    {< Folders for the game images. }
    property ImageCaptions: TStringList read FImageCaptions;
    {< Captions for the folders of game's images. }

    // Texts
    // -----
    property InfoText: string read FInfoText write SetInfoText;
    {< Path to text file of the system. }

    property TextFolders: TStringList read FTextFolders;
    {< Folders for game texts. }
    property TextCaptions: TStringList read FTextCaptions;
    {< Captions for the folders of game's texts. }

    // Music
    // -----
    property MusicFolders: TStringList read FMusicFolders;
    {< Folders for game music. }
    property MusicCaptions: TStringList read FMusicCaptions;
    {< Captions for the folders of game's music. }

    // Video
    // -----
    property VideoFolders: TStringList read FVideoFolders;
    {< Folders for game videos. }
    property VideoCaptions: TStringList read FVideoCaptions;
    {< Captions for the folders of game's video. }

    // Import
    // ------
    property SoftExportKey: TEmutecaSoftExportKey
      read FSoftExportKey write SetSoftExportKey;
    {< Default key (CRC/SHA) to be used as game identifiers
       (when importing/exporting data). }
    property Extensions: TStringList read FExtensions;
    {< Extensions used by the system.

    Only one extension in every string, without dot.
    }

    property Stats: cEmutecaPlayingStats read FStats;

  end;

implementation

{ caEmutecaCustomSystem }

procedure caEmutecaCustomSystem.LoadFromIni(aIniFile: TIniFile);
begin
  if aIniFile = nil then
    Exit;

  // Basic data
  Title := aIniFile.ReadString(ID, krsIniKeyTitle, Title);

  FileName := aIniFile.ReadString(ID, krsIniKeyFileName, FileName);
  if FileName = '' then
    FileName := Title;

  Enabled := aIniFile.ReadBool(ID, krsIniKeyEnabled, Enabled);

  ExtractAll := aIniFile.ReadBool(ID, krsIniKeyExtractAll, ExtractAll);

  BaseFolder := aIniFile.ReadString(ID, krsIniKeyBaseFolder, BaseFolder);
  WorkingFolder := aIniFile.ReadString(ID, krsIniKeyWorkingFolder,
    WorkingFolder);

  // Emulators
  MainEmulator := aIniFile.ReadString(ID, krsIniKeyMainEmulator, MainEmulator);
  OtherEmulators.CommaText :=
    aIniFile.ReadString(ID, krsIniKeyOtherEmulators, OtherEmulators.CommaText);

  // Images
  Icon := aIniFile.ReadString(ID, krsIniKeyIcon, Icon);
  Image := aIniFile.ReadString(ID, krsIniKeyImage, Image);
  BackImage := aIniFile.ReadString(ID, krsIniKeyBackImage, BackImage);

  IconFolder := aIniFile.ReadString(ID, krsIniKeyIconFolder, IconFolder);
  ImageFolders.CommaText :=
    aIniFile.ReadString(ID, krsIniKeyImageFolders, ImageFolders.CommaText);
  ImageCaptions.CommaText :=
    aIniFile.ReadString(ID, krsIniKeyImageCaptions, ImageCaptions.CommaText);

  // Texts
  InfoText := aIniFile.ReadString(ID, krsIniKeyText, InfoText);

  TextFolders.CommaText :=
    aIniFile.ReadString(ID, krsIniKeyTextFolders, TextFolders.CommaText);
  TextCaptions.CommaText :=
    aIniFile.ReadString(ID, krsIniKeyTextCaptions, TextCaptions.CommaText);

  // Music
  MusicFolders.CommaText :=
    aIniFile.ReadString(ID, krsIniKeyMusicFolders, MusicFolders.CommaText);
  MusicCaptions.CommaText :=
    aIniFile.ReadString(ID, krsIniKeyMusicCaptions, MusicCaptions.CommaText);

  // Video
  VideoFolders.CommaText :=
    aIniFile.ReadString(ID, krsIniKeyVideoFolders, VideoFolders.CommaText);
  VideoCaptions.CommaText :=
    aIniFile.ReadString(ID, krsIniKeyVideoCaptions, VideoCaptions.CommaText);

  // Import
  SoftExportKey := Str2SoftExportKey(
    aIniFile.ReadString(ID, krsIniKeySoftExportKey,
    SoftExportKey2StrK(SoftExportKey)));
  Extensions.CommaText := aIniFile.ReadString(ID, krsIniKeyExtensions,
    Extensions.CommaText);

  Stats.LoadFromIni(aIniFile, ID);

  // Fixing lists...
  FixFolderListData(ImageFolders, ImageCaptions);
  FixFolderListData(TextFolders, TextCaptions);
  FixFolderListData(MusicFolders, MusicCaptions);
  FixFolderListData(VideoFolders, VideoCaptions);
end;

procedure caEmutecaCustomSystem.SaveToIni(aIniFile: TMemIniFile;
  const ExportMode: boolean);
begin
  if aIniFile = nil then
    Exit;

  // Basic data
  aIniFile.WriteString(ID, krsIniKeyTitle, Title);
  aIniFile.WriteString(ID, krsIniKeyFileName, FileName);
  aIniFile.WriteBool(ID, krsIniKeyExtractAll, ExtractAll);

  // Emulators
  aIniFile.WriteString(ID, krsIniKeyMainEmulator, MainEmulator);
  aIniFile.WriteString(ID, krsIniKeyOtherEmulators, OtherEmulators.CommaText);

  // Import
  aIniFile.WriteString(ID, krsIniKeySoftExportKey,
    SoftExportKey2StrK(SoftExportKey));
  aIniFile.WriteString(ID, krsIniKeyExtensions, Extensions.CommaText);

  if ExportMode then
  begin // Las borramos por si acaso existen
    // Basic data
    aIniFile.DeleteKey(ID, krsIniKeyEnabled);
    aIniFile.DeleteKey(ID, krsIniKeyBaseFolder);
    aIniFile.DeleteKey(ID, krsIniKeyWorkingFolder);

    // Images
    aIniFile.DeleteKey(ID, krsIniKeyIcon);
    aIniFile.DeleteKey(ID, krsIniKeyImage);
    aIniFile.DeleteKey(ID, krsIniKeyBackImage);

    aIniFile.DeleteKey(ID, krsIniKeyIconFolder);
    aIniFile.DeleteKey(ID, krsIniKeyImageFolders);
    aIniFile.DeleteKey(ID, krsIniKeyImageCaptions);

    // Texts
    aIniFile.DeleteKey(ID, krsIniKeyText);

    aIniFile.DeleteKey(ID, krsIniKeyTextFolders);
    aIniFile.DeleteKey(ID, krsIniKeyTextCaptions);

    // Music
    aIniFile.DeleteKey(ID, krsIniKeyMusicFolders);
    aIniFile.DeleteKey(ID, krsIniKeyMusicCaptions);

    // Video
    aIniFile.DeleteKey(ID, krsIniKeyVideoFolders);
    aIniFile.DeleteKey(ID, krsIniKeyVideoCaptions);
  end
  else
  begin
    // Basic data
    aIniFile.WriteBool(ID, krsIniKeyEnabled, Enabled);
    aIniFile.WriteString(ID, krsIniKeyBaseFolder, BaseFolder);
    aIniFile.WriteString(ID, krsIniKeyWorkingFolder, WorkingFolder);

    // Images
    aIniFile.WriteString(ID, krsIniKeyIcon, Icon);
    aIniFile.WriteString(ID, krsIniKeyImage, Image);
    aIniFile.WriteString(ID, krsIniKeyBackImage, BackImage);

    aIniFile.WriteString(ID, krsIniKeyIconFolder, IconFolder);
    aIniFile.WriteString(ID, krsIniKeyImageFolders, ImageFolders.CommaText);
    aIniFile.WriteString(ID, krsIniKeyImageCaptions, ImageCaptions.CommaText);

    // Texts
    aIniFile.WriteString(ID, krsIniKeyText, InfoText);

    aIniFile.WriteString(ID, krsIniKeyTextFolders, TextFolders.CommaText);
    aIniFile.WriteString(ID, krsIniKeyTextCaptions, TextCaptions.CommaText);

    // Music
    aIniFile.WriteString(ID, krsIniKeyMusicFolders, MusicFolders.CommaText);
    aIniFile.WriteString(ID, krsIniKeyMusicCaptions, MusicCaptions.CommaText);

    // Video
    aIniFile.WriteString(ID, krsIniKeyVideoFolders, VideoFolders.CommaText);
    aIniFile.WriteString(ID, krsIniKeyVideoCaptions, VideoCaptions.CommaText);
  end;

  Stats.WriteToIni(aIniFile, ID, ExportMode);
end;

procedure caEmutecaCustomSystem.CacheIcon(aImagList: cCHXImageList);
begin
  if not Assigned(aImagList) then
    Exit;
  if Assigned(Stats.Icon) then
    Exit;

  if FileExistsUTF8(Icon) then
  begin
    Stats.Icon := aImagList[aImagList.AddImageFile(Icon)];
  end
  else
  begin
    // aImagList[2] is default for systems
    if aImagList.Count > 3 then
    begin
      Stats.Icon := aImagList[3];
    end
    else
    begin
      if aImagList.Count > 0 then
      begin
        Stats.Icon := aImagList[aImagList.Count - 1];
      end
      else
      begin
        Stats.Icon := aImagList[aImagList.AddImageFile(Icon)];
      end;
    end;
  end;
end;

procedure caEmutecaCustomSystem.SetBaseFolder(AValue: string);
begin
  FBaseFolder := SetAsFolder(AValue);
end;

procedure caEmutecaCustomSystem.SetBackImage(AValue: string);
begin
  FBackImage := SetAsFile(AValue);
end;

procedure caEmutecaCustomSystem.SetEnabled(AValue: boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
end;

procedure caEmutecaCustomSystem.SetExtractAll(AValue: boolean);
begin
  if FExtractAll = AValue then
    Exit;
  FExtractAll := AValue;
end;

procedure caEmutecaCustomSystem.SetFileName(AValue: string);
begin
  FFileName := CleanFileName(AValue);
end;

procedure caEmutecaCustomSystem.SetSoftExportKey(
  AValue: TEmutecaSoftExportKey);
begin
  if FSoftExportKey = AValue then
    Exit;
  FSoftExportKey := AValue;
end;

procedure caEmutecaCustomSystem.SetIcon(AValue: string);
begin
  FIcon := SetAsFile(AValue);
end;

procedure caEmutecaCustomSystem.SetIconFolder(AValue: string);
begin
  FIconFolder := SetAsFolder(AValue);
end;

procedure caEmutecaCustomSystem.SetID(AValue: string);
begin
  if FID = AValue then
    Exit;
  FID := AValue;

  FPONotifyObservers(Self, ooChange, nil);
end;

procedure caEmutecaCustomSystem.SetImage(AValue: string);
begin
  FImage := SetAsFile(AValue);
end;

procedure caEmutecaCustomSystem.SetInfoText(AValue: string);
begin
  FInfoText := SetAsFile(AValue);
end;

procedure caEmutecaCustomSystem.SetMainEmulator(AValue: string);
begin
  if FMainEmulator = AValue then
    Exit;
  FMainEmulator := AValue;

  if OtherEmulators.IndexOf(MainEmulator) = -1 then
    OtherEmulators.Add(MainEmulator);
end;

procedure caEmutecaCustomSystem.SetTempFolder(AValue: string);
begin
  FTempFolder := SetAsFolder(AValue) + SetAsFolder(ID);
end;

procedure caEmutecaCustomSystem.SetWorkingFolder(AValue: string);
begin
  FWorkingFolder := SetAsFolder(AValue);
end;

procedure caEmutecaCustomSystem.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
end;

procedure caEmutecaCustomSystem.FixFolderListData(FolderList,
  CaptionList: TStrings);
var
  i: integer;
begin
  // Removing empty Folders and asociated captions.
  i := 0;
  while i < FolderList.Count do
  begin
    FolderList[i] := SetAsFolder(FolderList[i]);
    if FolderList[i] = '' then
    begin
      FolderList.Delete(i);
      if i < CaptionList.Count then
        CaptionList.Delete(i);
    end
    else
      Inc(i);
  end;

  // Adding text (folder name) to empty Captions
  if FolderList.Count > CaptionList.Count then
  begin
    i := CaptionList.Count;
    while i < FolderList.Count do
    begin
      CaptionList.Add(ExtractFileName(ExcludeTrailingPathDelimiter(
        FolderList[i])));
      Inc(i);
    end;
  end;

  // Removing exceed of captions
  while FolderList.Count < CaptionList.Count do
    CaptionList.Delete(CaptionList.Count - 1);
end;

function caEmutecaCustomSystem.MatchID(aID: string): boolean;
begin
  Result := CompareID(aID) = 0;
end;

function caEmutecaCustomSystem.CompareID(aID: string): integer;
begin
  Result := UTF8CompareText(Self.ID, aID);
end;

constructor caEmutecaCustomSystem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Enabled := False;
  SoftExportKey := TEFKSHA1;

  FExtensions := TStringList.Create;
  Extensions.CaseSensitive := False;
  Extensions.Sorted := True;

  FOtherEmulators := TStringList.Create;
  OtherEmulators.CaseSensitive := False;
  OtherEmulators.Sorted := True;

  FImageCaptions := TStringList.Create;
  FImageFolders := TStringList.Create;

  FTextCaptions := TStringList.Create;
  FTextFolders := TStringList.Create;

  FMusicCaptions := TStringList.Create;
  FMusicFolders := TStringList.Create;

  FVideoCaptions := TStringList.Create;
  FVideoFolders := TStringList.Create;

  FStats := cEmutecaPlayingStats.Create(Self);
end;

destructor caEmutecaCustomSystem.Destroy;
begin
  Extensions.Free;
  OtherEmulators.Free;

  ImageCaptions.Free;
  ImageFolders.Free;

  TextCaptions.Free;
  TextFolders.Free;

  MusicCaptions.Free;
  MusicFolders.Free;

  VideoCaptions.Free;
  VideoFolders.Free;

  Stats.Free;

  inherited Destroy;
end;

initialization
  RegisterClass(caEmutecaCustomSystem);

finalization
  UnRegisterClass(caEmutecaCustomSystem);
end.
