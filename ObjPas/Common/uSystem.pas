{ This file is part of Emuteca

  Copyright (C) 2006-2012 Chixpy

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

{cSystem unit}
unit uSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, FileUtil, fgl;

type

  { @name.

    @definitionList(
    @itemLabel(NOTE:)
    @item(Because PascalScript don't suport overloaded methods,
      we don't use them right here.)
    )
  }

  { cSystem }

  cSystem = class
  private
    FBackgroundImage: String;
    FBaseFolder: String;
    FBeginYear: String;
    FCompany: String;
    FDataFile: String;
    FDemoMusicFolder: String;
    FDemoVideoFolder: String;
    FImageModes: TStringList;
    FMarqueeFolder: String;
    FMusicModes: TStringList;
    FMusicParameters: TStringList;
    FOtherEmulators: TStringList;
    FEnabled: boolean;
    FEndYear: String;
    FExtensions: TStringList;
    FExtractAll: boolean;
    FLastYear: String;
    FGameFolder: String;
    FIcon: String;
    FIconFolder: String;
    FImage: String;
    FImageCaptions: TStringList;
    FImageFolders: TStringList;
    FFirstYear: String;
    FMainEmulator: String;
    FModel: String;
    FMusicCaptions: TStringList;
    FMusicExecutables: TStringList;
    FMusicExtensions: TStringList;
    FMusicFolders: TStringList;
    FID: String;
    FRecursiveGameFolder: boolean;
    FTempFolder: String;
    FInfoText: String;
    FTextCaptions: TStringList;
    FTextFolders: TStringList;
    FTextModes: TStringList;
    FUseCRC: boolean;
    FVideoCaptions: TStringList;
    FVideoExecutables: TStringList;
    FVideoExtensions: TStringList;
    FVideoFolders: TStringList;
    FVideoModes: TStringList;
    FVideoParameters: TStringList;
    procedure SetBackgroundImage(AValue: String);
    procedure SetBaseFolder(const AValue: String);
    procedure SetBeginYear(const AValue: String);
    procedure SetCompany(const AValue: String);
    procedure SetDataFile(const AValue: String);
    procedure SetDemoMusicFolder(const AValue: String);
    procedure SetDemoVideoFolder(const AValue: String);
    procedure SetMarqueeFolder(const AValue: String);
    procedure SetEnabled(const AValue: boolean);
    procedure SetEndYear(const AValue: String);
    procedure SetExtractAll(const AValue: boolean);
    procedure SetLastYear(const AValue: String);
    procedure SetGameFolder(const AValue: String);
    procedure SetIcon(const AValue: String);
    procedure SetIconFolder(const AValue: String);
    procedure SetImage(const AValue: String);
    procedure SetFirstYear(const AValue: String);
    procedure SetMainEmulator(const AValue: String);
    procedure SetModel(const AValue: String);
    procedure SetID(const AValue: String);
    procedure SetRecursiveGameFolder(const AValue: boolean);
    procedure SetTempFolder(const AValue: String);
    procedure SetInfoText(const AValue: String);
    procedure SetUseCRC(const AValue: boolean);
  protected
    procedure FixFolderListData(FolderList, CaptionList, ModeList: TStrings);
    {< Try to fix some incosistences in folders, captions and modes data.

    FolderList, CaptionList and ModeList must be the images or texts ones.
    Maybe in the future for vídeos and music.

    System Manager tries, but who knows if somebody edited
      the .ini file...
    }

  public
    property Enabled: boolean read FEnabled write SetEnabled;
    //< Is the system enabled?

    property ID: String read FID write SetID;
    //< Name or ID of the system (usually, Company + Model).
    property Company: String read FCompany write SetCompany;
    //< Company of the system.
    property Model: String read FModel write SetModel;
    //< Model of the system.
    property FirstYear: String read FFirstYear write SetFirstYear;
    //< Year of release
    property LastYear: String read FLastYear write SetLastYear;
    //< Year of discontinuation
    property Extensions: TStringList read FExtensions;
    {< Extensions used by the system and its emulators

    Only one extension in every string, without dot.
    }

    property Icon: String read FIcon write SetIcon;
    //< Path to the icon of the system.
    property Image: String read FImage write SetImage;
    //< Path to image of the system.
    property BackgroundImage: String read FBackgroundImage write SetBackgroundImage;
    //< Image used for list background
    property InfoText: String read FInfoText write SetInfoText;
    //< Path to text file of the system.

    property DataFile: String read FDataFile write SetDataFile;
    {< Name of the files with the game/group data.

    This is the name of the files of games and groups data. No full path,
      no extension; only name. These files are handled by cGameManager, not
      cSystem.
    }

    property ExtractAll: boolean read FExtractAll write SetExtractAll;
    //< Must all files be extracted from compressed archives?
    property UseCRC: boolean read FUseCRC write SetUseCRC;
    //< Must CRC be used as game identifiers (when importing/exporting data)
    property TempFolder: String read FTempFolder write SetTempFolder;
    //< Custom temporal folder for decompress game files.

    property BaseFolder: String read FBaseFolder write SetBaseFolder;
    {< System base folder

      Folder where all data and subfolders reside. Used by default to store
        some data if the file isn't defined (System image or text.)
    }
    property GameFolder: String read FGameFolder write SetGameFolder;
    //< Folder for games of the system.
    property RecursiveGameFolder: boolean
      read FRecursiveGameFolder write SetRecursiveGameFolder;
    //< Search in the subfolder of the game folder?

    property IconFolder: String read FIconFolder write SetIconFolder;
    //< Folder for the icons of the games.
    property MarqueeFolder: String read FMarqueeFolder
      write SetMarqueeFolder;
    //< Folder for marquees (or box spine).
    property ImageFolders: TStringList read FImageFolders;
    //< Folders for the game images.
    property ImageCaptions: TStringList read FImageCaptions;
    //< Captions for the folders of game's images.
    property ImageModes: TStringList read FImageModes;
    {< File search mode for the folders of game's images.

      @definitionList(
        @itemLabel("0")
        @item(Single image. Search in the folder only one image file with
          the game's name.)

        @itemLabel("1")
        @item(Multiple image. Search in the folder all files in a subfolder
          with the game's name. Something like MAME's style.)
      )
    }
    property TextFolders: TStringList read FTextFolders;
    //< Folders for game texts.
    property TextCaptions: TStringList read FTextCaptions;
    //< Captions for the folders of game's texts.
    property TextModes: TStringList read FTextModes;
    {< Text search mode for the folders of game's text.

      @definitionList(
        @itemLabel("0")
        @item(Single text. Search in the folder only one text file with
          the game's name.)

        @itemLabel("1")
        @item(Multiple texts. Search in the folder all files in a subfolder
          with the game's name. Something like MAME's style.)
      )
    }

    property DemoMusicFolder: String
      read FDemoMusicFolder write SetDemoMusicFolder;
    {< Folder with demo music of the games.

       This folder is inteded to store the music that will be played when
         selecting a game or group.
    }
    property MusicFolders: TStringList read FMusicFolders;
    //< Folders for game music (OST, remixes).
    property MusicCaptions: TStringList read FMusicCaptions;
    //< Captions for the folders of game's music.
    property MusicExtensions: TStringList read FMusicExtensions;
    //< Extensions to search in the folders (separated with comma as usual).
    property MusicExecutables: TStringList read FMusicExecutables;
    //< Executables for play music files (i.e. call a external program).
    property MusicParameters: TStringList read FMusicParameters;
    //< Parameter for executables.
    property MusicModes: TStringList read FMusicModes;
    {< Music search mode for the folders of game's music.

      @definitionList (
        @itemLabel("0")
        @item(Single text. Search in the folder only one music file with
          the game's name.)

        @itemLabel("1")
        @item(Multiple texts. Search in the folder all files in a subfolder
          with the game's name. Something like MAME's style.)
      )
    }

    property DemoVideoFolder: String
      read FDemoVideoFolder write SetDemoVideoFolder;
    {< Folder with demo videos of the games.

       This folder is inteded to store the videos that will be played when
         selecting a game or group.
    }
    property VideoFolders: TStringList read FVideoFolders;
    //< Folders for game videos (Speedruns, TAS, etc.).
    property VideoCaptions: TStringList read FVideoCaptions;
    //< Captions for the folders of game's videos.
    property VideoExtensions: TStringList read FVideoExtensions;
    //< Extensions to search in the folders (separated with comma as usual).
    property VideoExecutables: TStringList read FVideoExecutables;
    //< Commandline for play video files (i.e. call a external program).
    property VideoParameters: TStringList read FVideoParameters;
    //< Parameters for executables.
    property VideoModes: TStringList read FVideoModes;
    {< Video search mode for the folders of game's music.

      @definitionList(
        @itemLabel("0")
        @item(Single text. Search in the folder only one video file with
          the game's name.)

        @itemLabel("1")
        @item(Multiple texts. Search in the folder all files in a subfolder
          with the game's name. Something like MAME's style.)
      )
    }

    property MainEmulator: String
      read FMainEmulator write SetMainEmulator;
    //< Main emulator ID.
    property OtherEmulators: TStringList read FOtherEmulators;
    //< Ids of other emulators for the system.

    procedure LoadFromFile(const IniFile: String);
    {< Loads system data from a ini file.

      @param (IniFile A Ini file name.)
    }
    procedure LoadFromFileIni(IniFile: TCustomIniFile);
    {< Loads system data from a ini file.

      @param (IniFile An already opened TInifile.)
    }
    procedure SaveToFile(const IniFile: String;
      const ExportMode: boolean = False);
    {< Saves system data to a Ini file.

      @param (IniFile A Ini file name.)
      @param (ExportMode Is it for export data? Don't write personal settings
        as folders or files.)
    }
    procedure SaveToFileIni(IniFile: TCustomIniFile;
      const ExportMode: boolean = False);
    {< Saves system data to a Ini file.

      @param (IniFile An already opened TInifile.)
      @param (ExportMode Is it for export data? Don't write personal settings
        as folders or files.)
    }

    constructor Create(const AName: String);
    {< Create a new system.

      @param (AName Name/Key of the system.)
    }

    destructor Destroy; override;
  end;

  cSystemList = specialize TFPGObjectList<cSystem>;

implementation

uses
  uCustomUtils;

{ cSystem }

procedure cSystem.SetBaseFolder(const AValue: String);
begin
  FBaseFolder := SetAsFolder(AValue);
end;

procedure cSystem.SetBackgroundImage(AValue: String);
begin
  FBackgroundImage := AValue;
end;

procedure cSystem.SetBeginYear(const AValue: String);
begin
  FBeginYear := AValue;
end;

procedure cSystem.SetCompany(const AValue: String);
begin
  FCompany := AValue;
end;

procedure cSystem.SetDataFile(const AValue: String);
begin
  FDataFile := AValue;
end;

procedure cSystem.SetDemoMusicFolder(const AValue: String);
begin
  FDemoMusicFolder := SetAsFolder(AValue);
end;

procedure cSystem.SetDemoVideoFolder(const AValue: String);
begin
  FDemoVideoFolder := SetAsFolder(AValue);
end;

procedure cSystem.SetMarqueeFolder(const AValue: String);
begin
  FMarqueeFolder := SetAsFolder(AValue);
end;

procedure cSystem.SetEnabled(const AValue: boolean);
begin
  FEnabled := AValue;
end;

procedure cSystem.SetEndYear(const AValue: String);
begin
  FEndYear := AValue;
end;

procedure cSystem.SetExtractAll(const AValue: boolean);
begin
  FExtractAll := AValue;
end;

procedure cSystem.SetLastYear(const AValue: String);
begin
  FLastYear := AValue;
end;

procedure cSystem.SetGameFolder(const AValue: String);
begin
  FGameFolder := SetAsFolder(AValue);
end;

procedure cSystem.SetIcon(const AValue: String);
begin
  FIcon := AValue;
end;

procedure cSystem.SetIconFolder(const AValue: String);
begin
  FIconFolder := SetAsFolder(AValue);
end;

procedure cSystem.SetImage(const AValue: String);
begin
  FImage := AValue;
end;

procedure cSystem.SetFirstYear(const AValue: String);
begin
  FFirstYear := AValue;
end;

procedure cSystem.SetMainEmulator(const AValue: String);
begin
  FMainEmulator := AValue;
end;

procedure cSystem.SetModel(const AValue: String);
begin
  FModel := AValue;
end;

procedure cSystem.SetID(const AValue: String);
begin
  FID := Trim(AValue);

  if DataFile <> '' then
    Exit;
  DataFile := CleanFileName(FID);
end;

procedure cSystem.SetRecursiveGameFolder(const AValue: boolean);
begin
  FRecursiveGameFolder := AValue;
end;

procedure cSystem.SetTempFolder(const AValue: String);
begin
  FTempFolder := SetAsFolder(AValue);
end;

procedure cSystem.SetInfoText(const AValue: String);
begin
  FInfoText := AValue;
end;

procedure cSystem.SetUseCRC(const AValue: boolean);
begin
  FUseCRC := AValue;
end;

procedure cSystem.FixFolderListData(FolderList, CaptionList,
  ModeList: TStrings);
var
  i: integer;
begin
  // Folders
  i := 0;
  while i < FolderList.Count do
  begin
    FolderList[i] := SetAsFolder(FolderList[i]);
    if FolderList[i] = '' then
      FolderList.Delete(i)
    else
      Inc(i);
  end;

  // Captions
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

  while FolderList.Count < CaptionList.Count do
    CaptionList.Delete(CaptionList.Count - 1);

  // Modes
  if FolderList.Count > ModeList.Count then
  begin
    i := ModeList.Count;
    while i < FolderList.Count do
    begin
      ModeList.Add(BoolToStr(False));
      Inc(i);
    end;
  end;

  while FolderList.Count < ModeList.Count do
    ModeList.Delete(ModeList.Count - 1);
end;

procedure cSystem.LoadFromFile(const IniFile: String);
var
  F: TMemInifile;
begin
  if not FileExistsUTF8(IniFile) then
    Exit;
  F := TMemIniFile.Create(UTF8ToSys(IniFile));
  try
    LoadFromFileIni(F);
  finally
    FreeAndNil(F);
  end;
end;

procedure cSystem.LoadFromFileIni(IniFile: TCustomIniFile);
begin
  if IniFile = nil then
    Exit;

  Enabled := IniFile.ReadBool(ID, 'Enabled', Enabled);

  Company := IniFile.ReadString(ID, 'Company', Company);
  Model := IniFile.ReadString(ID, 'Model', Model);
  FirstYear := IniFile.ReadString(ID, 'FirstYear', FirstYear);
  LastYear := IniFile.ReadString(ID, 'LastYear', LastYear);
  Extensions.CommaText := IniFile.ReadString(ID, 'Extensions', Extensions.CommaText);
  TempFolder := IniFile.ReadString(ID, 'TempFolder', TempFolder);

  Icon := IniFile.ReadString(ID, 'Icon', Icon);
  Image := IniFile.ReadString(ID, 'Image', Image);
  BackgroundImage := IniFile.ReadString(ID, 'BackgroundImage', BackgroundImage);
  InfoText := IniFile.ReadString(ID, 'Text', InfoText);

  DataFile := IniFile.ReadString(ID, 'DataFile', DataFile);

  BaseFolder := IniFile.ReadString(ID, 'BaseFolder', BaseFolder);
  GameFolder := IniFile.ReadString(ID, 'GameFolder', GameFolder);
  RecursiveGameFolder := IniFile.ReadBool(ID, 'RecursiveGameFolder', RecursiveGameFolder);

  UseCRC := IniFile.ReadBool(ID, 'UseCRC', UseCRC);
  ExtractAll := IniFile.ReadBool(ID, 'ExtractAll', ExtractAll);
  TempFolder := IniFile.ReadString(ID, 'TempFolder', TempFolder);

  IconFolder := IniFile.ReadString(ID, 'IconFolder', IconFolder);
  MarqueeFolder := IniFile.ReadString(ID, 'MarqueeFolder', MarqueeFolder);
  ImageFolders.CommaText := IniFile.ReadString(ID, 'ImageFolders', ImageFolders.CommaText);
  ImageCaptions.CommaText := IniFile.ReadString(ID, 'ImageCaptions', ImageCaptions.CommaText);
  ImageModes.CommaText := IniFile.ReadString(ID, 'ImageModes', ImageModes.CommaText);

  TextFolders.CommaText := IniFile.ReadString(ID, 'TextFolders', TextFolders.CommaText);
  TextCaptions.CommaText :=IniFile.ReadString(ID, 'TextCaptions', TextCaptions.CommaText);
  TextModes.CommaText := IniFile.ReadString(ID, 'TextModes', TextModes.CommaText);

  DemoMusicFolder := IniFile.ReadString(ID, 'DemoMusicFolder', DemoMusicFolder);
  MusicFolders.CommaText := IniFile.ReadString(ID, 'MusicFolders', MusicFolders.CommaText);
  MusicCaptions.CommaText := IniFile.ReadString(ID, 'MusicCaptions', MusicCaptions.CommaText);
  MusicModes.CommaText := IniFile.ReadString(ID, 'MusicModes', MusicModes.CommaText);
  MusicExtensions.CommaText := IniFile.ReadString(ID, 'MusicExtensions', MusicExtensions.CommaText);
  MusicExecutables.CommaText := IniFile.ReadString(ID, 'MusicExecutables', MusicExecutables.CommaText);
  MusicParameters.CommaText := IniFile.ReadString(ID, 'MusicParameters', MusicParameters.CommaText);

  DemoVideoFolder := IniFile.ReadString(ID, 'DemoVideoFolder', DemoVideoFolder);
  VideoFolders.CommaText := IniFile.ReadString(ID, 'VideoFolders', VideoFolders.CommaText);
  VideoCaptions.CommaText := IniFile.ReadString(ID, 'VideoCaptions', VideoCaptions.CommaText);
  VideoModes.CommaText := IniFile.ReadString(ID, 'VideoModes', VideoModes.CommaText);
  VideoExtensions.CommaText := IniFile.ReadString(ID, 'VideoExtensions', VideoExtensions.CommaText);
  VideoExecutables.CommaText := IniFile.ReadString(ID, 'VideoExecutables', VideoExecutables.CommaText);
  VideoParameters.CommaText := IniFile.ReadString(ID, 'VideoParameters', VideoParameters.CommaText);

  MainEmulator := IniFile.ReadString(ID, 'MainEmulator', MainEmulator);
  OtherEmulators.CommaText := IniFile.ReadString(ID, 'OtherEmulators', OtherEmulators.CommaText);

  // Reparamos posibles incosistencias...
  FixFolderListData(ImageFolders, ImageCaptions, ImageModes);
  FixFolderListData(TextFolders, TextCaptions, TextModes);

  // TODO 4: Ough...
  FixFolderListData(MusicFolders, MusicCaptions, MusicModes);
  FixFolderListData(MusicFolders, MusicExtensions, MusicModes);
  FixFolderListData(MusicFolders, MusicExecutables, MusicModes);
  FixFolderListData(VideoFolders, VideoCaptions, VideoModes);
  FixFolderListData(VideoFolders, VideoExtensions, VideoModes);
  FixFolderListData(VideoFolders, VideoExecutables, VideoModes);
end;

procedure cSystem.SaveToFile(const IniFile: String;
  const ExportMode: boolean);
var
  F: TMemInifile;
begin
  F := TMemIniFile.Create(UTF8ToSys(IniFile));
  try
    SaveToFileIni(F, ExportMode);
    F.UpdateFile;
  finally
    FreeAndNil(F);
  end;
end;

procedure cSystem.SaveToFileIni(IniFile: TCustomIniFile;
  const ExportMode: boolean);
begin
  // Reparamos posibles incosistencias...
  FixFolderListData(ImageFolders, ImageCaptions, ImageModes);
  FixFolderListData(TextFolders, TextCaptions, TextModes);

  if IniFile = nil then
    Exit;

  IniFile.WriteString(ID, 'Company', Company);
  IniFile.WriteString(ID, 'Model', Model);
  IniFile.WriteString(ID, 'FirstYear', FirstYear);
  IniFile.WriteString(ID, 'LastYear', LastYear);
  IniFile.WriteString(ID, 'Extensions', Extensions.CommaText);

  IniFile.WriteBool(ID, 'RecursiveGameFolder', RecursiveGameFolder);
  IniFile.WriteBool(ID, 'UseCRC', UseCRC);
  IniFile.WriteBool(ID, 'ExtractAll', ExtractAll);

  IniFile.WriteString(ID, 'MainEmulator', MainEmulator);
  IniFile.WriteString(ID, 'OtherEmulators', OtherEmulators.CommaText);

  if ExportMode then
  begin // Las borramos por si acaso existen
    IniFile.DeleteKey(ID, 'Enabled');

    IniFile.DeleteKey(ID, 'Icon');
    IniFile.DeleteKey(ID, 'Image');
    IniFile.DeleteKey(ID, 'BackgroundImage');
    IniFile.DeleteKey(ID, 'Text');

    IniFile.DeleteKey(ID, 'DataFile');

    IniFile.DeleteKey(ID, 'TempFolder');
    IniFile.DeleteKey(ID, 'BaseFolder');
    IniFile.DeleteKey(ID, 'GameFolder');

    IniFile.DeleteKey(ID, 'IconFolder');
    IniFile.DeleteKey(ID, 'MarqueeFolder');
    IniFile.DeleteKey(ID, 'ImageFolders');
    IniFile.DeleteKey(ID, 'ImageCaptions');
    IniFile.DeleteKey(ID, 'ImageModes');

    IniFile.DeleteKey(ID, 'TextFolders');
    IniFile.DeleteKey(ID, 'TextCaptions');
    IniFile.DeleteKey(ID, 'TextModes');

    IniFile.DeleteKey(ID, 'DemoMusicFolder');
    IniFile.DeleteKey(ID, 'MusicFolders');
    IniFile.DeleteKey(ID, 'MusicCaptions');
    IniFile.DeleteKey(ID, 'MusicExtensions');
    IniFile.DeleteKey(ID, 'MusicExecutables');
    IniFile.DeleteKey(ID, 'MusicParameters');

    IniFile.DeleteKey(ID, 'DemoVideoFolder');
    IniFile.DeleteKey(ID, 'VideoFolders');
    IniFile.DeleteKey(ID, 'VideoCaptions');
    IniFile.DeleteKey(ID, 'VideoExtensions');
    IniFile.DeleteKey(ID, 'VideoExecutables');
    IniFile.DeleteKey(ID, 'VideoParameters');
  end
  else
  begin
    IniFile.WriteBool(ID, 'Enabled', Enabled);

    IniFile.WriteString(ID, 'Icon', Icon);
    IniFile.WriteString(ID, 'Image', Image);
    IniFile.WriteString(ID, 'BackgroundImage', BackgroundImage);
    IniFile.WriteString(ID, 'Text', InfoText);

    IniFile.WriteString(ID, 'DataFile', DataFile);

    IniFile.WriteString(ID, 'TempFolder', TempFolder);
    IniFile.WriteString(ID, 'BaseFolder', BaseFolder);
    IniFile.WriteString(ID, 'GameFolder', GameFolder);

    IniFile.WriteString(ID, 'IconFolder', IconFolder);
    IniFile.WriteString(ID, 'MarqueeFolder', MarqueeFolder);
    IniFile.WriteString(ID, 'ImageFolders', ImageFolders.CommaText);
    IniFile.WriteString(ID, 'ImageCaptions', ImageCaptions.CommaText);
    IniFile.WriteString(ID, 'ImageModes', ImageModes.CommaText);
    IniFile.WriteString(ID, 'TextFolders', TextFolders.CommaText);
    IniFile.WriteString(ID, 'TextCaptions', TextCaptions.CommaText);
    IniFile.WriteString(ID, 'TextModes', TextModes.CommaText);

    IniFile.WriteString(ID, 'DemoMusicFolder', DemoMusicFolder);
    IniFile.WriteString(ID, 'MusicFolders', MusicFolders.CommaText);
    IniFile.WriteString(ID, 'MusicCaptions', MusicFolders.CommaText);
    IniFile.WriteString(ID, 'MusicExtensions', MusicExtensions.CommaText);
    IniFile.WriteString(ID, 'MusicExecutables', MusicExecutables.CommaText);
    IniFile.WriteString(ID, 'MusicParameters', MusicParameters.CommaText);

    IniFile.WriteString(ID, 'DemoVideoFolder', DemoVideoFolder);
    IniFile.WriteString(ID, 'VideoFolders', VideoFolders.CommaText);
    IniFile.WriteString(ID, 'VideoCaptions', VideoCaptions.CommaText);
    IniFile.WriteString(ID, 'VideoExtensions', VideoExtensions.CommaText);
    IniFile.WriteString(ID, 'VideoExecutables', VideoExecutables.CommaText);
    IniFile.WriteString(ID, 'VideoParameters', VideoExecutables.CommaText);
  end;
end;

constructor cSystem.Create(const AName: String);
begin
  inherited Create;
  Self.ID := AName;
  Self.DataFile := CleanFileName(AName);

  Self.Enabled := False;
  Self.RecursiveGameFolder := True;
  Self.UseCRC := True;

  Self.FExtensions := TStringList.Create;
  Self.FExtensions.CaseSensitive := False;

  Self.FOtherEmulators := TStringList.Create;

  Self.FImageCaptions := TStringList.Create;
  Self.FImageFolders := TStringList.Create;
  Self.FImageModes := TStringList.Create;

  Self.FTextCaptions := TStringList.Create;
  Self.FTextFolders := TStringList.Create;
  Self.FTextModes := TStringList.Create;

  Self.FMusicCaptions := TStringList.Create;
  Self.FMusicExtensions := TStringList.Create;
  Self.FMusicExtensions.CaseSensitive := False;
  Self.FMusicFolders := TStringList.Create;
  Self.FMusicModes := TStringList.Create;
  Self.FMusicExecutables := TStringList.Create;
  Self.FMusicParameters := TStringList.Create;

  Self.FVideoCaptions := TStringList.Create;
  Self.FVideoExtensions := TStringList.Create;
  Self.FVideoExtensions.CaseSensitive := False;
  Self.FVideoFolders := TStringList.Create;
  Self.FVideoModes := TStringList.Create;
  Self.FVideoExecutables := TStringList.Create;
  Self.FVideoParameters := TStringList.Create;
end;

destructor cSystem.Destroy;
begin
  FreeAndNil(Self.FExtensions);
  FreeAndNil(Self.FOtherEmulators);

  FreeAndNil(Self.FImageCaptions);
  FreeAndNil(Self.FImageFolders);
  FreeAndNil(Self.FImageModes);

  FreeAndNil(Self.FTextCaptions);
  FreeAndNil(Self.FTextFolders);
  FreeAndNil(Self.FTextModes);

  FreeAndNil(Self.FMusicCaptions);
  FreeAndNil(Self.FMusicExtensions);
  FreeAndNil(Self.FMusicFolders);
  FreeAndNil(Self.FMusicModes);
  FreeAndNil(Self.FMusicExecutables);
  FreeAndNil(Self.FMusicParameters);

  FreeAndNil(Self.FVideoCaptions);
  FreeAndNil(Self.FVideoExtensions);
  FreeAndNil(Self.FVideoFolders);
  FreeAndNil(Self.FVideoModes);
  FreeAndNil(Self.FVideoExecutables);
  FreeAndNil(Self.FVideoParameters);

  inherited Destroy;
end;

end.

