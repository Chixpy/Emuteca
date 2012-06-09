{ This file is part of Emuteca Front End.

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

{ Unit of Game Manager form }
unit fGameManager;

{$mode objfpc}{$H+}
{$codepage utf8}
interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Windows, Graphics,
  Dialogs, ExtCtrls, ComCtrls, Menus, ActnList, StdCtrls, Buttons, Clipbrd,
  contnrs, VirtualTrees, VTHeaderPopup, lclintf, LCLType, LazHelpHTML,
  IniPropStorage, IDEWindowIntf, dateutils, strutils, LazUTF8,
  uGameManager, uConfig, uCustomUtils, uImageList, uSystemManager,
  uGame, uGameGroup, uGameStats, u7zWrapper, uVersionSupport, fSystemManager,
  fEmulatorManager, fImageViewer, fScriptManager, fMediaManager, fProgress,
  fAbout, fConfigManager;

const
  CDBExt = '.edb';
  CDBFilter = ' (*' + CDBExt + ')|*' + CDBExt;

type
  TlvGroupMode = (
    lvGMGameGroup, //< Group by game.
    lvGMName,      //< Group by version name (SortName property).
    lvGMYear,      //< Group by year (published).
    lvGMDeveloper, //< Group by developer.
    lvGMPublisher, //< Group by publisher.
    lvGMFolder,    //< Group by folder (or 7z).
    lvGMTags       //< Group by tags.
    );
  {< Modes for grouping the games. }

  { Form for Game Manager (Main Form). }

  { TfrmGameManager }

  TfrmGameManager = class(TForm)
    actExit: TAction;
    actEmulatorManager: TAction;
    actGroupByFamily: TAction;
    actGroupByYear: TAction;
    actGroupByDeveloper: TAction;
    actGroupByPublisher: TAction;
    actGroupByName: TAction;
    actGroupByFolder: TAction;
    actGroupByTags: TAction;
    actEmulatorWebPage: TAction;
    actEmulatorFolder: TAction;
    actAbout: TAction;
    actExportSystemData: TAction;
    actImportSystemData: TAction;
    actConfigManager: TAction;
    actChangeGameListFont: TAction;
    actPasteGameIconImage: TAction;
    actPasteGameSpineImage: TAction;
    actSaveGameList: TAction;
    actSaveEmuText: TAction;
    actLockEmuText: TAction;
    actSaveGameText: TAction;
    actSaveSystemText: TAction;
    actPurgeSystemData: TAction;
    actSaveProperties: TAction;
    actPreviousGameText: TAction;
    actNextGameText: TAction;
    actViewEmulatorImage: TAction;
    actShowHelp: TAction;
    actPasteGameImage: TAction;
    actLockGameText: TAction;
    actLockSystemText: TAction;
    actPreviousGameImage: TAction;
    actNextGameImage: TAction;
    actOpenSystemFolder: TAction;
    actShowGroupTypeMenu: TAction;
    actSaveEditorData: TAction;
    actViewGameImage: TAction;
    actMediaManager: TAction;
    actScriptManager: TAction;
    actViewSystemImage: TAction;
    actQuickUpdateList: TAction;
    actUpdateGameList: TAction;
    actRunEmulator: TAction;
    actPlayGame: TAction;
    actSystemManager: TAction;
    actShowMainMenu: TAction;
    ActionList: TActionList;
    bEditorSave: TBitBtn;
    bSaveProperties: TButton;
    cbEmulators: TComboBox;
    cbGameGroup: TComboBox;
    cbGameImages: TComboBox;
    cbGameTexts: TComboBox;
    cbPublisher: TComboBox;
    cbSearch: TComboBox;
    cbSystem: TComboBox;
    cbYear: TComboBox;
    chkVerified: TCheckBox;
    eAlternate: TEdit;
    eBadDump: TEdit;
    eCracked: TEdit;
    eFixed: TEdit;
    eHack: TEdit;
    eLanguages: TEdit;
    eLicense: TEdit;
    eModified: TEdit;
    ePirate: TEdit;
    eSortKey: TEdit;
    eName: TEdit;
    eSearch: TEdit;
    eTrainer: TEdit;
    eTranslation: TEdit;
    eVersion: TEdit;
    eZones: TEdit;
    FontDialog: TFontDialog;
    HTMLBrowserHelpViewer: THTMLBrowserHelpViewer;
    HTMLHelpDatabase: THTMLHelpDatabase;
    iGameImage: TImage;
    iEmulatorImage: TImage;
    ilActions: TImageList;
    IniPropStorage: TIniPropStorage;
    lCracked: TLabel;
    lHack: TLabel;
    lImageCount: TLabel;
    lLicense: TLabel;
    lFixed: TLabel;
    lModified: TLabel;
    lPirate: TLabel;
    lTextCount: TLabel;
    lTranslation: TLabel;
    lTrainer: TLabel;
    lBadDump: TLabel;
    lAlternate: TLabel;
    lLanguages: TLabel;
    lGameGroup: TLabel;
    lGameTags: TLabel;
    lGroupTags: TLabel;
    lSortKey: TLabel;
    lName: TLabel;
    lPublisher: TLabel;
    lVersion: TLabel;
    lYear: TLabel;
    lZones: TLabel;
    memoEmulator: TMemo;
    miPasteIconImage: TMenuItem;
    miPasteSpineGame: TMenuItem;
    miPasteGameImage: TMenuItem;
    miPasteClipboardImage: TMenuItem;
    miFileSeparator: TMenuItem;
    miChangeGameListFont: TMenuItem;
    miSaveGameList: TMenuItem;
    miConfigManager: TMenuItem;
    miGMSearchGameInternet: TMenuItem;
    miGMSep1: TMenuItem;
    miGMPlayGame: TMenuItem;
    miSep2: TMenuItem;
    miPurgeSystemData: TMenuItem;
    miExportSystemData: TMenuItem;
    miImportSystemData: TMenuItem;
    miViewEmulatorImage: TMenuItem;
    miEmulatorFolder: TMenuItem;
    miEmulatorWebPage: TMenuItem;
    memoSystem: TMemo;
    memoGame: TMemo;
    miSep1: TMenuItem;
    miHelpOnLine: TMenuItem;
    mmGameTags: TMemo;
    mmGroupTags: TMemo;
    mVSTGames: TMenuItem;
    miAbout: TMenuItem;
    miHelp: TMenuItem;
    miNextGameImage: TMenuItem;
    miPreviousGameImage: TMenuItem;
    miSearchEmulatorInternet: TMenuItem;
    miSearchSystemInternet: TMenuItem;
    miSearchGameInternet: TMenuItem;
    miOpenSystemFolder: TMenuItem;
    OpenDialog: TOpenDialog;
    pcGame: TPageControl;
    pagGameMedia: TTabSheet;
    pagOtherFiles: TTabSheet;
    pmEmulatorImage: TPopupMenu;
    pmGameList: TPopupMenu;
    pnlEmulatorImage: TPanel;
    pnlGameText: TPanel;
    pnlSystemImage: TPanel;
    pnlSystemText: TPanel;
    pYear: TPanel;
    pZone: TPanel;
    pZoneYear: TPanel;
    pagEmulator: TTabSheet;
    SaveDialog: TSaveDialog;
    splEmulator: TSplitter;
    pagEditor: TTabSheet;
    pagProperties: TTabSheet;
    tbEmulatorImage: TToolBar;
    tbEmulatorText: TToolBar;
    tbGameImage: TToolBar;
    bViewGameImage: TToolButton;
    tbSystemImage: TToolBar;
    bSepTBGameImg1: TToolButton;
    bPreviousGameImage: TToolButton;
    bNextGameImage: TToolButton;
    iSystemImage: TImage;
    miMediaManager: TMenuItem;
    miGroupByTags2: TMenuItem;
    miGroupByFolder2: TMenuItem;
    miGroupByName2: TMenuItem;
    miGroupByPublisher2: TMenuItem;
    miGroupByDeveloper2: TMenuItem;
    miGroupByYear2: TMenuItem;
    miGroupByFamily2: TMenuItem;
    miGroupByTags: TMenuItem;
    miGroupByFolder: TMenuItem;
    miGroupByName: TMenuItem;
    miGroupByPublisher: TMenuItem;
    miGroupByDeveloper: TMenuItem;
    miGroupByYear: TMenuItem;
    miGroupByFamily: TMenuItem;
    miView: TMenuItem;
    miViewGameImage: TMenuItem;
    miScriptManager: TMenuItem;
    miViewSystemImage: TMenuItem;
    miTools: TMenuItem;
    pcLeft: TPageControl;
    pmGameImage: TPopupMenu;
    pmSystemImage: TPopupMenu;
    pmGroupType: TPopupMenu;
    miEmulator: TMenuItem;
    miEmulatorManager: TMenuItem;
    miQuickUpdateList: TMenuItem;
    miUpdateGameList: TMenuItem;
    miRunEmulator: TMenuItem;
    miPlayGame: TMenuItem;
    miGame: TMenuItem;
    miSystemManager: TMenuItem;
    miSystem: TMenuItem;
    miFile: TMenuItem;
    miExit: TMenuItem;
    pnlGameImage: TPanel;
    pnlSearch: TPanel;
    pnlCenter: TPanel;
    pmMainMenu: TPopupMenu;
    splSystemPanel: TSplitter;
    splGamePanel: TSplitter;
    splSystemImage: TSplitter;
    splGameMedia: TSplitter;
    sbHelp: TStatusBar;
    sbInfo: TStatusBar;
    pagSystem: TTabSheet;
    tbMain: TToolBar;
    bMenu: TToolButton;
    tbSystemText: TToolBar;
    tbTexts: TToolBar;
    bSepTBMainTB1: TToolButton;
    bPlayGame: TToolButton;
    bRunEmulator: TToolButton;
    bSepTBMainTB2: TToolButton;
    bSysteManager: TToolButton;
    bEmulatorManager: TToolButton;
    bMediaManager: TToolButton;
    bScriptManager: TToolButton;
    bSepTBMainTB3: TToolButton;
    bGroupType: TToolButton;
    bLockSystemText: TToolButton;
    bLockGameText: TToolButton;
    bLockEmuText: TToolButton;
    bViewEmulatorImage: TToolButton;
    bPreviousGameText: TToolButton;
    bNextGameText: TToolButton;
    bSepTBSystemText1: TToolButton;
    bSepTBGameText1: TToolButton;
    bViewSystemImage: TToolButton;
    bPasteGameImage: TToolButton;
    bSepTBGameImg2: TToolButton;
    bSaveEmuText: TToolButton;
    bSaveGameText: TToolButton;
    bSaveSystemText: TToolButton;
    VTHGamesPopupMenu: TVTHeaderPopupMenu;
    vstFiles: TVirtualStringTree;
    vstGames: TVirtualStringTree;
    procedure actAboutExecute(Sender: TObject);
    procedure actChangeGameListFontExecute(Sender: TObject);
    procedure actConfigManagerExecute(Sender: TObject);
    procedure actEmulatorFolderExecute(Sender: TObject);
    procedure actEmulatorManagerExecute(Sender: TObject);
    procedure actEmulatorWebPageExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actExportSystemDataExecute(Sender: TObject);
    procedure actGroupByDeveloperExecute(Sender: TObject);
    procedure actGroupByFamilyExecute(Sender: TObject);
    procedure actGroupByFolderExecute(Sender: TObject);
    procedure actGroupByNameExecute(Sender: TObject);
    procedure actGroupByPublisherExecute(Sender: TObject);
    procedure actGroupByTagsExecute(Sender: TObject);
    procedure actGroupByYearExecute(Sender: TObject);
    procedure actImportSystemDataExecute(Sender: TObject);
    procedure actLockGameTextExecute(Sender: TObject);
    procedure actLockSystemTextExecute(Sender: TObject);
    procedure actMediaManagerExecute(Sender: TObject);
    procedure actNextGameImageExecute(Sender: TObject);
    procedure actOpenSystemFolderExecute(Sender: TObject);
    procedure actPasteGameIconImageExecute(Sender: TObject);
    procedure actPasteGameImageExecute(Sender: TObject);
    procedure actPasteGameSpineImageExecute(Sender: TObject);
    procedure actPlayGameExecute(Sender: TObject);
    procedure actPreviousGameImageExecute(Sender: TObject);
    procedure actPurgeSystemDataExecute(Sender: TObject);
    procedure actQuickUpdateListExecute(Sender: TObject);
    procedure actRunEmulatorExecute(Sender: TObject);
    procedure actSaveEditorDataExecute(Sender: TObject);
    procedure actSaveEmuTextExecute(Sender: TObject);
    procedure actSaveGameListExecute(Sender: TObject);
    procedure actSaveGameTextExecute(Sender: TObject);
    procedure actSavePropertiesExecute(Sender: TObject);
    procedure actSaveSystemTextExecute(Sender: TObject);
    procedure actScriptManagerExecute(Sender: TObject);
    procedure actShowGroupTypeMenuExecute(Sender: TObject);
    procedure actShowMainMenuExecute(Sender: TObject);
    procedure actSystemManagerExecute(Sender: TObject);
    procedure actUpdateGameListExecute(Sender: TObject);
    procedure actViewEmulatorImageExecute(Sender: TObject);
    procedure actViewGameImageExecute(Sender: TObject);
    procedure actViewSystemImageExecute(Sender: TObject);
    procedure cbEmulatorsSelect(Sender: TObject);
    procedure cbGameGroupSelect(Sender: TObject);
    procedure cbGameImagesChange(Sender: TObject);
    procedure cbGameTextsChange(Sender: TObject);
    procedure cbSearchChange(Sender: TObject);
    procedure cbSystemDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure cbSystemSelect(Sender: TObject);
    procedure ePropertiesKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure eEditorKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure eNameChange(Sender: TObject);
    procedure eSearchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tbLockSystemTextClick(Sender: TObject);
    procedure tbLockGameTextClick(Sender: TObject);
    procedure vstFilesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstFilesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstFilesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstGamesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstGamesCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
    procedure vstGamesDblClick(Sender: TObject);
    procedure vstGamesDrawText(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellText: string; const CellRect: TRect; var DefaultDraw: boolean);
    procedure vstGamesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);

  private
    FConfig: cConfig;
    FCurrGame: cGame;
    FCurrGroup: cGameGroup;
    FGameIcons: cImageList;
    FGameImages: TStringList;
    FGameImagesIndex: integer;
    FGameManager: cGameManager;
    FGameTexts: TStringList;
    FGameTextsIndex: integer;
    FGroupIcons: cImageList;
    FGroupList: TFPObjectList;
    FGroupMode: TlvGroupMode;
    FrsAddingFile: string;
    FrsAssignToGroup: string;
    FrsChooseImageFileFormat: string;
    FrsConfirmOverwriteFile: string;
    FrsDecompressing: string;
    FrsDeveloper: string;
    FrsEmutecaGameDatabase: string;
    FrsErrorEmulator: string;
    FrsExportingData: string;
    FrsFilename: string;
    FrsErrorGameNotFound: string;
    FrsImportingData: string;
    FrsKey: string;
    FrsLoadingGameList: string;
    FrsNever: string;
    FrsNGames: string;
    FrsNGroups: string;
    FrsNTimes: string;
    FrsPublisher: string;
    FrsPurgeMessage: string;
    FrsSavingGameList: string;
    FrsZones: string;
    FrsUnknown: string;
    FrsUpdatingList: string;
    FrsVersion: string;
    FSystemIcons: cImageList;
    FTempFolder: string;
    FVerInfoIcons: cImageList;
    FZoneIcons: cImageList;
    FZoneList: TStringList;
    procedure SetConfig(const AValue: cConfig);
    procedure SetCurrGame(const AValue: cGame);
    procedure SetCurrGroup(const AValue: cGameGroup);
    procedure SetGameIcons(const AValue: cImageList);
    procedure SetGameImagesIndex(const AValue: integer);
    procedure SetGameManager(const AValue: cGameManager);
    procedure SetGameTextsIndex(AValue: integer);
    procedure SetGroupIcons(const AValue: cImageList);
    procedure SetGroupList(const AValue: TFPObjectList);
    procedure SetGroupMode(const AValue: TlvGroupMode);
    procedure SetrsAddingFile(const AValue: string);
    procedure SetrsAssignToGroup(const AValue: string);
    procedure SetrsChooseImageFileFormat(const AValue: string);
    procedure SetrsConfirmOverwriteFile(AValue: string);
    procedure SetrsDecompressing(const AValue: string);
    procedure SetrsDeveloper(const AValue: string);
    procedure SetrsEmutecaGameDatabase(AValue: string);
    procedure SetrsErrorEmulator(AValue: string);
    procedure SetrsExportingData(const AValue: string);
    procedure SetrsFilename(const AValue: string);
    procedure SetrsErrorGameNotFound(const AValue: string);
    procedure SetrsImportingData(const AValue: string);
    procedure SetrsKey(const AValue: string);
    procedure SetrsLoadingGameList(const AValue: string);
    procedure SetrsNever(const AValue: string);
    procedure SetrsNGames(const AValue: string);
    procedure SetrsNGroups(const AValue: string);
    procedure SetrsNTimes(const AValue: string);
    procedure SetrsPublisher(const AValue: string);
    procedure SetrsPurgeMessage(AValue: string);
    procedure SetrsSavingGameList(const AValue: string);
    procedure SetrsZones(const AValue: string);
    procedure SetrsUnknown(const AValue: string);
    procedure SetrsUpdatingList(const AValue: string);
    procedure SetrsVersion(const AValue: string);
    procedure SetSystemIcons(const AValue: cImageList);
    procedure SetTempFolder(const AValue: string);

  protected
    // Strings for translation
    // -----------------------

    property rsAddingFile: string read FrsAddingFile write SetrsAddingFile;
    //< Localizable string: 'Adding file:'.
    property rsUpdatingList: string read FrsUpdatingList write SetrsUpdatingList;
    //< Localizable string: 'Updating list:'.
    property rsLoadingGameList: string read FrsLoadingGameList
      write SetrsLoadingGameList;
    //< Localizable string: 'Loading game list:'.
    property rsSavingGameList: string read FrsSavingGameList write SetrsSavingGameList;
    //< Localizable string: 'Saving game list:'.
    property rsDecompressing: string read FrsDecompressing write SetrsDecompressing;
    //< Localizable string: 'Decompressing:'.
    property rsEmutecaGameDatabase: string
      read FrsEmutecaGameDatabase write SetrsEmutecaGameDatabase;
    property rsImportingData: string read FrsImportingData write SetrsImportingData;
    //< Localizable string: 'Importing data:'.
    property rsExportingData: string read FrsExportingData write SetrsExportingData;
    //< Localizable string: 'Exporting data:'.
    property rsKey: string read FrsKey write SetrsKey;
    //< Localizable string: 'Key'.
    property rsZones: string read FrsZones write SetrsZones;
    //< Localizable string: 'Sort name'.
    property rsDeveloper: string read FrsDeveloper write SetrsDeveloper;
    //< Localizable string: 'Developer'.
    property rsPublisher: string read FrsPublisher write SetrsPublisher;
    //< Localizable string: 'Publisher'.
    property rsVersion: string read FrsVersion write SetrsVersion;
    //< Localizable string: 'Version'.
    property rsFilename: string read FrsFilename write SetrsFilename;
    //< Localizable string: 'Filename'.
    property rsNGroups: string read FrsNGroups write SetrsNGroups;
    //< Localizable string: '%d groups'.
    property rsNGames: string read FrsNGames write SetrsNGames;
    //< Localizable string: '%d games'.
    property rsNTimes: string read FrsNTimes write SetrsNTimes;
    //< Localizable string: '%d times'.
    property rsNever: string read FrsNever write SetrsNever;
    //< Localizable string: 'Never'.
    property rsUnknown: string read FrsUnknown write SetrsUnknown;
    //< Localizable string: '!Unknown'.
    property rsAssignToGroup: string read FrsAssignToGroup write SetrsAssignToGroup;
    //< 'Do you want to assign it to the game''s group?'.
    property rsChooseImageFileFormat: string
      read FrsChooseImageFileFormat write SetrsChooseImageFileFormat;
    {< Localizable string for chosing image format.
      'Do you want to save it in a lossless format:
        YES -> .png (lossless for screenshots)
        NO -> .jpg (better for photographs)'
    }
    property rsConfirmOverwriteFile: string
      read FrsConfirmOverwriteFile write SetrsConfirmOverwriteFile;
    {< Localizable string to confirm overwrite a file.
      ' %s
      The file already exists.
      Do you want overwrite it?'
    }
    property rsErrorGameNotFound: string read FrsErrorGameNotFound
      write SetrsErrorGameNotFound;
    //< Localizable string: 'Game not found: %s / %s'.
    property rsErrorEmulator: string read FrsErrorEmulator write SetrsErrorEmulator;
    //< Localizable string: 'Emulator exited with errorcode: %d'.
    property rsPurgeMessage: string read FrsPurgeMessage write SetrsPurgeMessage;
    //< Purge data warning.

    property GroupMode: TlvGroupMode read FGroupMode write SetGroupMode;
    //< Listing mode used in vstGames.

    property GroupList: TFPObjectList read FGroupList write SetGroupList;
    {< List with the groups for vstGames.

      Here are the groups with the data for vstGames.

      @definitionList(
        @itemLabel(NOTE:)
        @item(In TfrmGameManager, a group refers to the groups used
          for listing the games. Don't confuse with cGameManger game groups.)
      )
    }

    property CurrGame: cGame read FCurrGame write SetCurrGame;
    //< Current selected game version.
    property CurrGroup: cGameGroup read FCurrGroup write SetCurrGroup;
    {< Current seleted GameGroup or the GameGroup of selected game version. }
    property GameImages: TStringList read FGameImages;
    //< Images of the current selected item.
    property GameImagesIndex: integer read FGameImagesIndex write SetGameImagesIndex;
    //< Index of the current Image.
    property GameTexts: TStringList read FGameTexts;
    //< Texts of the current selected item.
    property GameTextsIndex: integer read FGameTextsIndex write SetGameTextsIndex;
    //< Index of the current Text.

    property GameManager: cGameManager read FGameManager;
    //< Game Manager object.
    property Config: cConfig read FConfig;
    //< Current Config object.
    property SystemIcons: cImageList read FSystemIcons;
    //< Icons of the systems.
    property GroupIcons: cImageList read FGroupIcons;
    //< Group icons.
    property GameIcons: cImageList read FGameIcons;
    //< Game icons (exclusive ones).
    property VerInfoIcons: cImageList read FVerInfoIcons;
    //< Icons for version column.
    property ZoneIcons: cImageList read FZoneIcons;
    //< Icons for zones.
    property ZoneList: TStringList read FZoneList;
    //< Keys for zones
    property TempFolder: string read FTempFolder write SetTempFolder;
    //< Temp folder.

    function Group(const aIndex: integer): cGameGroup;
    {< Gets a Group by index.

      Usually used for iterations. Remember, it begins from 0 to
        [GroupCount - 1].

      @definitionList(
        @itemLabel(NOTE:)
        @item(In TfrmGameManager, a group refers to the groups used
          for listing the games. Don't confuse with cGameManager.GroupAtPos.)
      )

      @param(aIndex The position on the list of groups.)
      @return(The cGameGroup in this position. @nil if it don't exists,
        in other words, aIndex is out of range.)
    }
    function Group(aGameGroupID: string): cGameGroup;
    {< Gets a Group by ID.

      @definitionList(
        @itemLabel(NOTE:)
        @item(In TfrmGameManager, a group refers to the groups used
          for listing the games. Don't confuse with cGameManager.Group.)
      )
      @param(aGameGroupID ID of the group.)
      @return(The cGameGroup with the ID. @nil if it don't exists.)
    }
    function GroupCount: longint;
    {< Gets the number of te item in the group list.

      @definitionList(
        @itemLabel(NOTE:)
        @item(In TfrmGameManager, a group refers to the groups used
          for listing the games. Don't confuse with cGameManager.GroupCount.)
      )
      @return(Number of the Groups.)
    }
    function AddGroupVTV(aGameGroupID: string): PVirtualNode;
    //< Adds a group to vstGames.
    procedure UpdateGroupNodeTempData(Node: PVirtualNode);
    //< Updates number of childs (versions), time played, last time, etc.
    procedure UpdateVTVGameList;
    //< Updates the vstGames

    procedure UpdateSystemList;
    //< Updates de system list (cbSystem.Items).
    procedure ChangeCurrentSystem;
    //< Change the current selected system in cbSystem.

    procedure UpdateSystemMedia;
    //< Updates the system media.
    procedure UpdateSystemMediaCaptions;
    //< Updates cbGameImages, cbGameTexts, etc.
    procedure UpdateSystemImage;
    //< Loads the system image.
    procedure UpdateSystemBackground;
    //< Loads the system background for game lists.
    procedure UpdateSystemText;
    //< Loads the system text.

    procedure UpdateGameMedia;
    //< Updates the game media.
    procedure UpdateGameProperties;
    //< Updates the game properties.
    procedure UpdateGameImage;
    //< Changes the game image.
    procedure UpdateGameText;
    //< Changes the game text.
    procedure UpdateGameEditFields;
    //< Updates the fields of editor with game data.
    procedure SaveGameData;
    //< Saves the game data.
    procedure ClearGameMedia;
    //< Clears the game media fields.
    procedure ClearProperties;
    //< Clears the game properties fields.

    procedure UpdateGroupMedia;
    //< Updates de group media.
    procedure UpdateGroupImage;
    //< Changes the group image.
    procedure UpdateGroupText;
    //< Changes the group text.
    procedure UpdateGroupEditFields;
    //< Updates the fields of editor with group data.
    procedure SaveGroupData;
    //< Saves the group data.

    procedure UpdateEmulatorList;
    //< Updates the emulator list.
    procedure ChangeCurrentEmulator;
    //< Changes the current selected emulator.
    procedure UpdateEmulatorMedia;
    //< Updates de emulator media.
    procedure UpdateEmulatorImage;
    //< Changes the emulator image.
    procedure UpdateEmulatorText;
    //< Changes the emulator text.

    procedure NextGameImage;
    //< Change to the next game image.
    procedure PreviousGameImage;
    //< Change to the previous game image.

    procedure PasteGameImage;
    //< Pastes and assings the image in the clipboard to the game/group.
    procedure PasteGameSpineImage;
    //< Pastes and assings the game spine image in the clipboard to the game/group.
    procedure PasteGameIconImage;
    //< Pastes and assings the game icon in the clipboard to the game/group.
    procedure SaveGameText;
    //< Saves the game text.
    procedure LoadImageFromClipboard(aPicture: TPicture);
    {< Assigns the image from clipcboard and assings it to a TPicture.
    }
    procedure SaveImageToFile(aPicture: TPicture; aFilename: string);
    {< Saves a TPicture to a file.
    }
    procedure SaveTextToFile(aText: TStrings; aFilename: string);
    {< Saves a TStrings to a file.
    }

    procedure FillOtherFilesTree;
    {< Fills other files vst.
    }

    procedure SearchInInternet(Sender: TObject);
    {< Opens a searcher web page.

      Webpage is based on Hint property of Sender and the current
        item selected in the game list.
    }

    procedure SearchGroupText(StrList: TStrings; aGameGroup: cGameGroup);
    procedure SearchGroupImage(StrList: TStrings; aGameGroup: cGameGroup);
    procedure SearchGameText(aGame: cGame);
    procedure SearchGameImage(aGame: cGame);

    procedure SearchGames;
    //< Search games that contain eSearch.Text
    procedure ShowAllNodes(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: boolean);
    procedure HideNodes(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: boolean);

    procedure OpenGameImages;
    //< Open game images in Image Viewer
    procedure OpenSystemImages;
    //< Open system images in Image Viewer
    procedure OpenSystemFolder;
    //< Open system folder with explorer
    procedure OpenEmulatorImages;
    //< Open emulator images in Image Viewer
    procedure OpenEmulatorFolder;
    //< Open emulator folder with explorer
    procedure ShowImage(aFileName: string; aViewer: TImage);
    //< Show image in the TImage
    procedure ShowText(aFileName: string; aViewer: TMemo);
    //< Show text file in the TMemo

    function AddZoneIcon(Folder: string; Info: TSearchRec): boolean;

    function GMProgressCall(const TypeCB: TGMCallBackType;
      const Info1, Info2: string; const Value, Max: int64): boolean;
  public
    { public declarations }

  end;

var
  frmGameManager: TfrmGameManager;

implementation

{ TfrmGameManager }

procedure TfrmGameManager.FormCreate(Sender: TObject);

  procedure Translate;

  begin
    ;
  end;

  procedure DefaultConfig;
  var
    ImgExt: string;
  begin
    // Searching suportted image file types...
    ImgExt := UTF8LowerCase(GraphicFileMask(TGraphic));
    ImgExt := AnsiReplaceText(ImgExt, '*.', '');
    ImgExt := AnsiReplaceText(ImgExt, ';', ',');
    // ImgExt -> 'bmp,xpm,png,pbm,pgm,ppm,ico,icns,cur,jpg,jpeg,jpe,jfif, etc'
    // sortened to (if the extension exists)
    // ImgExt -> 'mng,png,gif,jng,jpg,jpe,jpeg,jfif' + others
    if UTF8Pos(',jfif', ImgExt) <> 0 then
      ImgExt := 'jfif,' + AnsiReplaceText(ImgExt, ',jfif', '');
    if UTF8Pos(',jpeg', ImgExt) <> 0 then // This must go before ',jpe'
      ImgExt := 'jpeg,' + AnsiReplaceText(ImgExt, ',jpeg', '');
    if UTF8Pos(',jpe', ImgExt) <> 0 then
      ImgExt := 'jpe,' + AnsiReplaceText(ImgExt, ',jpe', '');
    if UTF8Pos(',jpg', ImgExt) <> 0 then
      ImgExt := 'jpg,' + AnsiReplaceText(ImgExt, ',jpg', '');
    if UTF8Pos(',jng', ImgExt) <> 0 then
      ImgExt := 'jng,' + AnsiReplaceText(ImgExt, ',jng', '');
    if UTF8Pos(',gif', ImgExt) <> 0 then
      ImgExt := 'gif,' + AnsiReplaceText(ImgExt, ',gif', '');
    if UTF8Pos(',png', ImgExt) <> 0 then
      ImgExt := 'png,' + AnsiReplaceText(ImgExt, ',png', '');
    if UTF8Pos(',mng', ImgExt) <> 0 then
      ImgExt := 'mng,' + AnsiReplaceText(ImgExt, ',mng', '');

    with Config do
    begin
      // i18n
      LanguageFolder := 'i18n';
      LanguageFile := 'en.lng';
      HelpFolder := 'http://code.google.com/p/emuteca/wiki/';

      // Images
      ImagesFolder := SetAsFolder('Images');

      DefaultImagesSubfolder := SetAsFolder('Defaults');
      DefaultSystemImage := 'SystemImage.png';
      DefaultSystemIcon := 'SystemIcon.png';
      DefaultEmulatorImage := 'EmulatorImage.png';
      DefaultEmulatorIcon := 'EmulatorIcon.png';
      DefaultGameImage := 'GameImage.png';
      DefaultGameIcon := 'GameIcon.png';

      FlagsSubfolder := SetAsFolder('Flags');
      VIIconsSubfolder := SetAsFolder('VerInfo');
      IconsSubfolder := SetAsFolder('Icons');
      IconsIniFile := 'Icons.ini';

      // Config/Data
      SearchFile := 'Search.ini';
      DataFolder := SetAsFolder('Data');
      EmulatorsIniFile := 'Emulators.ini';
      SystemsIniFile := 'Systems.ini';

      // Tools
      ToolsFolder := SetAsFolder('Tools');
      z7Subfolder := SetAsFolder('7zip');
      z7CMExecutable := '7z.exe';
      z7GExecutable := '7zG.exe';
      mPlayerSubfolder := SetAsFolder('mplayer');
      mPlayerExecutable := 'mplayer2.exe';

      // CommonData folder
      CommonMediaFolder := SetAsFolder('Common');
      CompanySubFolder := SetAsFolder('Companies');
      YearSubFolder := SetAsFolder('Years');
      TagSubFolder := SetAsFolder('Tags');
      EmulatorSubFolder := SetAsFolder('Emulators');

      // Scripts
      ScriptsFolder := SetAsFolder('Scripts');
      GeneralScriptsSubFolder := SetAsFolder('General');
      GameScriptsSubFolder := SetAsFolder('Game');
      GroupScriptsSubFolder := SetAsFolder('Group');

      // File extensions
      GameDataExt := 'gam';
      GameGroupExt := 'fam';
      CompressedExtensions.CommaText := w7zFileExts;
      TextExtensions.CommaText := 'txt,nfo';
      ImageExtensions.CommaText := ImgExt;
      MusicExtensions.CommaText := 'ogg,mp3,wav,aac,flac';
      VideoExtensions.CommaText := 'mkv,mp4,avi,mpg';

      // Temp
      TempSubfolder := SetAsFolder('tEMpUTECA');
      TempFile := 'Emuteca.tmp';
    end;
  end;

  procedure SetupGameManager;
  begin
    FGameManager := cGameManager.Create(Config.DataFolder +
      Config.SystemsIniFile, TempFolder, Config.TempFile);
    GameManager.EmulatorsFile := Config.DataFolder + Config.EmulatorsIniFile;
    GameManager.TempFolder := Self.TempFolder;
    GameManager.GameDataFileExt := Config.GameDataExt;
    GameManager.GroupDataFileExt := Config.GameGroupExt;
    GameManager.ProgressCallBack := @Self.GMProgressCall;
  end;

  procedure LoadIcons;

    procedure AddVersionIcon(aImageList: cImageList; aIconFile: string);
    begin
      if FileExistsUTF8(aIconFile) then
        aImageList.AddImageFile(aIconFile)
      else
      if FileExistsUTF8(Config.ImagesFolder + Config.DefaultImagesSubfolder +
        Config.DefaultGameIcon) then
        GameIcons.AddImageFile(Config.ImagesFolder +
          Config.DefaultImagesSubfolder + Config.DefaultGameIcon)
      else
        GameIcons.AddEmptyImage;
    end;

  var
    aFolder: string;
  begin
    aFolder := Config.ImagesFolder + Config.IconsSubfolder;

    // Icons for menus (without assigned TAction)
    ReadMenuIcons(Config.IconsIniFile, Self.Name, aFolder,
      ilActions, pmMainMenu);
    ReadMenuIcons(Config.IconsIniFile, Self.Name, aFolder, ilActions,
      pmSystemImage);
    ReadMenuIcons(Config.IconsIniFile, Self.Name, aFolder, ilActions,
      pmGameImage);
    ReadMenuIcons(Config.IconsIniFile, Self.Name, aFolder, ilActions,
      pmGameList);

    // Icons for TActions
    ReadActionsIcons(Config.IconsIniFile, Self.Name, aFolder,
      ilActions, ActionList);

    // Zone icons
    aFolder := Config.ImagesFolder + Config.FlagsSubfolder;
    IterateFolderObj(aFolder, @AddZoneIcon, False);

    // Icons for "version" column
    // TODO 3: Make this list dinamic?
    aFolder := Config.ImagesFolder + Config.VIIconsSubfolder;
    AddVersionIcon(FVerInfoIcons, aFolder + 'NoZone.png');   // 0
    AddVersionIcon(FVerInfoIcons, aFolder + 'GoodDump.png');   // 1
    AddVersionIcon(FVerInfoIcons, aFolder + 'BadDump.png');    // 2
    AddVersionIcon(FVerInfoIcons, aFolder + 'NoAlternate.png');// 3
    AddVersionIcon(FVerInfoIcons, aFolder + 'Pirate.png');     // 4
    AddVersionIcon(FVerInfoIcons, aFolder + 'Traslation.png'); // 5
  end;

  procedure LoadSearchEngines;
  var
    aFile: TStringList;
    aSearcher: TStringList;
    i: integer;
    aPos: integer;
    aAction: TAction;
    aMenu: TMenuItem;
  begin
    aFile := TStringList.Create;
    aSearcher := TStringList.Create;

    try
      aFile.LoadFromFile(Config.SearchFile);

      i := 0;
      while i < aFile.Count do
      begin
        aPos := UTF8Pos('##', aFile[i]);
        if aPos <> 0 then
          aFile[i] := UTF8Copy(aFile[i], 1, aPos - 1);
        aSearcher.CommaText := aFile[i];
        Inc(i);

        if aSearcher.Count < 3 then
          Continue;
        aSearcher[0] := AnsiUpperCase(aSearcher[0]);

        // TODO 4: Repeated code...

        // Web searcher for Game
        aPos := UTF8Pos('G', aSearcher[0]);
        if aPos <> 0 then
        begin
          aAction := TAction.Create(nil); //< nil?
          aAction.Name := 'actSearcherG' + IntToStr(i);
          aAction.Caption := aSearcher[1];
          // Hint is used to show URL... and store it
          aAction.Hint := aSearcher[2];
          aAction.Tag := 1; //< Tag = 1 -> Search for Game
          aAction.Category := 'Game search';
          aAction.OnExecute := @SearchInInternet;
          aAction.ActionList := ActionList;

          // TODO 2: Add icon to the list and assign it to the action
          aMenu := TMenuItem.Create(nil);
          aMenu.Name := 'miSearcherG' + IntToStr(i);
          aMenu.Action := aAction;
          miSearchGameInternet.Add(aMenu);

          aMenu := TMenuItem.Create(nil);
          aMenu.Name := 'miGMSearcherG' + IntToStr(i);
          aMenu.Action := aAction;
          miGMSearchGameInternet.Add(aMenu);
        end;

        // Web searcher for System
        aPos := UTF8Pos('S', aSearcher[0]);
        if aPos <> 0 then
        begin
          aAction := TAction.Create(nil); //< nil?
          aAction.Name := 'actSearcherS' + IntToStr(i);
          aAction.Caption := aSearcher[1];
          // Hint is used to show URL... and store it
          aAction.Hint := aSearcher[2];
          aAction.Tag := 2; //< Tag = 2 -> Search for System
          aAction.Category := 'System search';
          aAction.OnExecute := @SearchInInternet;
          aAction.ActionList := ActionList;

          // TODO 2: Add icon to the list and assign it to the action

          aMenu := TMenuItem.Create(nil);
          aMenu.Name := 'miSearcherS' + IntToStr(i);
          aMenu.Action := aAction;
          miSearchSystemInternet.Add(aMenu);
        end;

        // Web searcher for Emulator
        aPos := UTF8Pos('E', aSearcher[0]);
        if aPos <> 0 then
        begin
          aAction := TAction.Create(nil); //< nil?
          aAction.Name := 'actSearcherE' + IntToStr(i);
          aAction.Caption := aSearcher[1];
          // Hint is used to show URL... and store it
          aAction.Hint := aSearcher[2];
          aAction.Tag := 3; //< Tag = 3 -> Search for Emulator
          aAction.Category := 'Emulator search';
          aAction.OnExecute := @SearchInInternet;
          aAction.ActionList := ActionList;

          // TODO 2: Add icon to the list and assign it to the action

          aMenu := TMenuItem.Create(nil);
          aMenu.Name := 'miSearcherE' + IntToStr(i);
          aMenu.Action := aAction;
          miSearchEmulatorInternet.Add(aMenu);
        end;
      end;
    finally
      FreeAndNil(aSearcher);
      FreeAndNil(aFile);
    end;
  end;

begin
  Randomize;
  // Always work from program folder :P
  // TODO 3: Change for Linux... ¬_¬U
  ChDir(ProgramDirectory);

  // Standard format setting (for .ini and other conversions)
  // This overrides user local settings that can cause errors
  DefaultFormatSettings.ThousandSeparator := ',';
  DefaultFormatSettings.DecimalSeparator := '.';
  DefaultFormatSettings.DateSeparator := '/';
  DefaultFormatSettings.TimeSeparator := ':';
  DefaultFormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  DefaultFormatSettings.ShortTimeFormat := 'hh:nn:ss';

  FConfig := cConfig.Create;
  DefaultConfig;
  Config.ReadConfig(Application.Title + '.ini');

  TempFolder := IncludeTrailingPathDelimiter(GetTempDir) + Config.TempSubfolder;
  ForceDirectoriesUTF8(TempFolder);

  // u7zWrapper vars;
  w7zPathTo7zGexe := Config.ToolsFolder + Config.z7Subfolder + Config.z7GExecutable;
  w7zPathTo7zexe := Config.ToolsFolder + Config.z7Subfolder + Config.z7CMExecutable;
  w7zFileExts := Config.CompressedExtensions.CommaText;

  Translate;
  Self.Caption := Application.Title + ' ' + GetFileVersion + ': ' + Self.Caption;

  HTMLHelpDatabase.BaseURL := Config.HelpFolder;

  FSystemIcons := cImageList.Create;
  FGameIcons := cImageList.Create;
  FGroupIcons := cImageList.Create;
  FVerInfoIcons := cImageList.Create;
  FZoneIcons := cImageList.Create;
  FZoneList := TStringList.Create;
  LoadIcons;

  LoadSearchEngines;

  SetupGameManager;

  // Initial pages (Sometimes I forgot restore they at design time XD )
  pcGame.PageIndex := 0;
  pcLeft.PageIndex := 0;
  // TODO 4: This is broken for unknown reason...
  //   cbSearch worked well before with translations :-/...
  cbSearch.ItemIndex := 0;

  FGroupList := TFPObjectList.Create(True);
  vstGames.NodeDataSize := SizeOf(TObject);
  vstGames.DefaultNodeHeight := abs(vstGames.Font.Height) * 2;

  vstFiles.NodeDataSize := SizeOf(string);
  FillOtherFilesTree;
  GroupMode := lvGMGameGroup;
  FGameImages := TStringList.Create;
  FGameTexts := TStringList.Create;
  UpdateSystemList;
end;

procedure TfrmGameManager.FormDestroy(Sender: TObject);
begin
  // Crappy segurity check... :-(
  if (Length(TempFolder) > Length(Config.TempSubfolder) + 1) and
    DirectoryExistsUTF8(TempFolder) then
    DeleteDirectory(TempFolder, False);

  FreeAndNil(FConfig);
  FreeAndNil(FSystemIcons);
  FreeAndNil(FGroupList);
  FreeAndNil(FZoneList);
  FreeAndNil(FZoneIcons);
  FreeAndNil(FVerInfoIcons);
  FreeAndNil(FGameIcons);
  FreeAndNil(FGroupIcons);
  FreeAndNil(FGameImages);
  FreeAndNil(FGameTexts);
  FreeAndNil(FGameManager);
end;

procedure TfrmGameManager.tbLockSystemTextClick(Sender: TObject);
begin
  memoSystem.ReadOnly := bLockSystemText.Enabled;
end;

procedure TfrmGameManager.tbLockGameTextClick(Sender: TObject);
begin
  memoGame.ReadOnly := bLockGameText.Enabled;
end;

procedure TfrmGameManager.vstFilesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if Node = nil then
    Exit;

  // TODO 1: Open the file...
end;

procedure TfrmGameManager.vstFilesFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  PData: ^string;
begin
  PData := Sender.GetNodeData(Node);
  Finalize(PData^);
end;

procedure TfrmGameManager.vstFilesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  PData: ^string;
begin
  PData := Sender.GetNodeData(Node);
  if PData^ = '' then
    Exit;
  case TextType of
    ttNormal: case Column of
        -1, 0: CellText := PData^;
      end;
    ttStatic: ;
  end;
end;

procedure TfrmGameManager.vstGamesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: ^TObject;
begin
  CurrGame := nil;
  CurrGroup := nil;

  if Sender.SelectedCount <> 1 then
    Exit;

  Data := Sender.GetNodeData(Node);

  if Data^ is cGameGroup then
  begin
    CurrGroup := cGameGroup(Data^);
    UpdateGroupMedia;
  end
  else
  if Data^ is cGame then
  begin
    CurrGame := cGame(Data^);
    CurrGroup := GameManager.Group(CurrGame.GameGroup);
    UpdateGameMedia;
  end;
end;

procedure TfrmGameManager.vstGamesCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
var
  Nodo1, Nodo2: ^TObject;
  aGame1, aGame2: cGame;
  aGroup1, aGroup2: cGameGroup;
begin
  Result := 0;
  Nodo1 := Sender.GetNodeData(Node1);
  Nodo2 := Sender.GetNodeData(Node2);

  if (Nodo1^ = nil) or (Nodo2^ = nil) then
    Exit;

  if (Nodo1^ is cGame) and (Nodo2^ is cGame) then
  begin
    aGame1 := cGame(Nodo1^);
    aGame2 := cGame(Nodo2^);

    case Column of
      -1, 0: // Name
        Result := UTF8CompareText(aGame1.SortKey, aGame2.SortKey);
      1: // Version
        Result := UTF8CompareText(aGame1.Version, aGame2.Version);
      2: // Publisher
        Result := UTF8CompareText(aGame1.Publisher, aGame2.Publisher);
      3: // Year
        Result := UTF8CompareText(aGame1.Year, aGame2.Year);
      4: // Time played
        Result := aGame1.PlayingTime - aGame2.PlayingTime;
      5: // Times
        Result := aGame1.TimesPlayed - aGame2.TimesPlayed;
      6: // Last time
        Result := Trunc(aGame1.LastTime - aGame2.LastTime);
      7: // Filename
        Result := UTF8CompareText(aGame1.FileName, aGame2.FileName);
      8: // Folder
        Result := UTF8CompareText(aGame1.Folder, aGame2.Folder);
    end;
  end
  else
  if (Nodo1^ is cGameGroup) and (Nodo2^ is cGameGroup) then
  begin
    aGroup1 := cGameGroup(Nodo1^);
    aGroup2 := cGameGroup(Nodo2^);

    case Column of
      -1, 0: // Name
        Result := UTF8CompareText(aGroup1.SortKey, aGroup2.SortKey);
      1: // Version
        Result := Node1^.ChildCount - Node2^.ChildCount;
      2: // Developer
        Result := UTF8CompareText(aGroup1.Developer, aGroup2.Developer);
      3: // Year
        Result := UTF8CompareText(aGroup1.Year, aGroup2.Year);
      4: // Time played
        Result := aGroup1.PlayingTime - aGroup2.PlayingTime;
      5: // Times
        Result := aGroup1.TimesPlayed - aGroup2.TimesPlayed;
      6: // Last time
        Result := Trunc(aGroup1.LastTime - aGroup2.LastTime);
      7: // Filename
        Result := UTF8CompareText(aGroup1.MediaFileName, aGroup2.MediaFileName);
      8: // Folder
        ;
    end;
  end;
end;

procedure TfrmGameManager.vstGamesDblClick(Sender: TObject);
begin
  actPlayGame.Execute;
end;

procedure TfrmGameManager.vstGamesDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: string; const CellRect: TRect; var DefaultDraw: boolean);
var
  Data: ^cGameStats;
  TmpGame: cGame;
  TmpStr: string;
  IconRect: TRect;
  StrList: TStringList;
  aPos: integer;
begin
  DefaultDraw := True;
  if (Node = nil) or (GameManager.System = nil) then
    Exit;
  Data := vstGames.GetNodeData(Node);
  if (Data^ = nil) then
    Exit;

  case Column of
    0:
    begin // Name column
      DefaultDraw := False;

      // Icon space
      IconRect := CellRect;
      IconRect.Right := IconRect.Left + IconRect.Bottom - IconRect.Top;

      // If icon is not set then search for it
      if Data^.IconIndex = -1 then
      begin
        StrList := TStringList.Create;
        try
          if Data^ is cGameGroup then
          begin
            case GroupMode of
              // Aprovechamos las ventajas del cGameManager :P
              lvGMYear: GameManager.SearchGroupMedia(StrList,
                  Config.CommonMediaFolder + SetAsFolder('Years') +
                  SetAsFolder('Icons'),
                  cGameGroup(Data^), Config.ImageExtensions, False, True);
              lvGMDeveloper: GameManager.SearchGroupMedia(StrList,
                  Config.CommonMediaFolder + SetAsFolder('Companies') +
                  SetAsFolder('Icons'), cGameGroup(Data^),
                  Config.ImageExtensions, False, True);
              lvGMPublisher: GameManager.SearchGroupMedia(StrList,
                  Config.CommonMediaFolder + SetAsFolder('Companies') +
                  SetAsFolder('Icons'), cGameGroup(Data^),
                  Config.ImageExtensions, False, True);
              { In folder mode... search in default location...

               lvGMFolder: GameManager.SearchGroupMedia(StrList,
                  Config.CommonMediaFolder + SetAsFolder('Folders') +
                  SetAsFolder('Icons'), cGameGroup(Data^), Config.ImageExtensions,
                  False, True);

              }
              lvGMTags: GameManager.SearchGroupMedia(StrList,
                  Config.CommonMediaFolder + SetAsFolder('Tags') +
                  SetAsFolder('Icons'),
                  cGameGroup(Data^), Config.ImageExtensions, False, True);
              else  // By default, standard search
                GameManager.SearchGroupMedia(StrList,
                  GameManager.System.IconFolder, cGameGroup(Data^),
                  Config.ImageExtensions, False, True);
            end;
          end
          else if Data^ is cGame then
            GameManager.SearchGameMedia(StrList, GameManager.System.IconFolder,
              cGame(Data^), Config.ImageExtensions, False, True);

          if StrList.Count > 0 then
          begin
            if StrList[0] <> '' then
              Data^.IconIndex := GameIcons.AddImageFile(StrList[0]);
          end;
          if Data^.IconIndex = -1 then
            Data^.IconIndex := 0;
        finally
          FreeAndNil(StrList);
        end;
      end;

      if (Data^.IconIndex > -1) and (Data^.IconIndex < GameIcons.Count) then
        TargetCanvas.StretchDraw(CorrectAspetRatio(IconRect,
          GameIcons[Data^.IconIndex]),
          GameIcons[Data^.IconIndex].Graphic);

      IconRect := CellRect;
      IconRect.Left := IconRect.Left + IconRect.Bottom - IconRect.Top +
        vstGames.TextMargin;

      DrawText(TargetCanvas.Handle, PChar(CellText), -1, IconRect,
        DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE or DT_WORDBREAK or
        DT_END_ELLIPSIS or DT_EDITCONTROL);
    end;

    1:
    begin // Version column
      if Data^ is cGame then
      begin
        DefaultDraw := False;

        // Icon space
        IconRect := CellRect;
        IconRect.Right := IconRect.Left + IconRect.Bottom - IconRect.Top;

        TmpGame := cGame(Data^);

        // TODO 1: Deal with multizone games
        aPos := -1;
        if TmpGame.Zones.Count > 0 then
          aPos := ZoneList.IndexOf(UTF8LowerCase(TmpGame.Zones[0]));
        if aPos <> -1 then
          TargetCanvas.StretchDraw(CorrectAspetRatio(IconRect,
            ZoneIcons[aPos]), ZoneIcons[aPos].Graphic)
        else
          TargetCanvas.StretchDraw(CorrectAspetRatio(IconRect,
            VerInfoIcons[0]), VerInfoIcons[0].Graphic);

        // Next position
        IconRect.Left := IconRect.Left + IconRect.Bottom - IconRect.Top;
        IconRect.Right := IconRect.Right + IconRect.Bottom - IconRect.Top;

        // Dump status
        if TmpGame.Verified then
        begin
          TargetCanvas.StretchDraw(CorrectAspetRatio(IconRect,
            VerInfoIcons[1]), VerInfoIcons[1].Graphic);
        end
        else if (TmpGame.BadDump <> '') or (TmpGame.Hack <> '') then
        begin
          TargetCanvas.StretchDraw(CorrectAspetRatio(IconRect,
            VerInfoIcons[2]), VerInfoIcons[2].Graphic);
        end
        else if TmpGame.Alternate = '' then
        begin
          TargetCanvas.StretchDraw(CorrectAspetRatio(IconRect,
            VerInfoIcons[3]), VerInfoIcons[3].Graphic);
        end;

        // Next position
        IconRect.Left := IconRect.Left + IconRect.Bottom - IconRect.Top;
        IconRect.Right := IconRect.Right + IconRect.Bottom - IconRect.Top;

        if (TmpGame.Pirate <> '') then
        begin
          TargetCanvas.StretchDraw(CorrectAspetRatio(IconRect,
            VerInfoIcons[4]), VerInfoIcons[4].Graphic);
          IconRect.Left := IconRect.Left + IconRect.Bottom - IconRect.Top;
          IconRect.Right := IconRect.Right + IconRect.Bottom - IconRect.Top;
        end;

        TmpStr := TmpGame.Translation;
        if (TmpStr <> '') then
        begin
          TargetCanvas.StretchDraw(CorrectAspetRatio(IconRect,
            VerInfoIcons[5]), VerInfoIcons[5].Graphic);

          if (TmpStr[1] = '+') or (TmpStr[1] = '-') then
            TmpStr := Trim(UTF8Copy(TmpStr, 2, 3))
          else
            TmpStr := Trim(UTF8Copy(TmpStr, 1, 3));

          //Drawing text over icon
          DrawText(TargetCanvas.Handle, PChar(TmpStr), -1, IconRect,
            DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE or
            DT_EDITCONTROL or DT_CENTER);
          IconRect.Left := IconRect.Left + IconRect.Bottom - IconRect.Top;
          IconRect.Right := IconRect.Right + IconRect.Bottom - IconRect.Top;
        end;


        IconRect.Right := CellRect.Right;
        IconRect.Top := CellRect.Top;
        IconRect.Bottom := CellRect.Bottom;
        IconRect.Left := IconRect.Left + vstGames.TextMargin;

        DrawText(TargetCanvas.Handle, PChar(CellText), -1, IconRect,
          DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE or DT_RIGHT or
          DT_WORDBREAK or DT_END_ELLIPSIS or DT_EDITCONTROL);
      end;
    end;
    else
      Exit;
  end;
end;

procedure TfrmGameManager.vstGamesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: ^TObject;
  TmpStr: string;
begin
  Data := vstGames.GetNodeData(Node);
  if Data^ = nil then
    Exit;

  if (Data^ is cGameGroup) and (TextType = ttNormal) then
  begin
    UpdateGroupNodeTempData(Node);

    with cGameGroup(Data^) do
    begin
      case Column of
        -1, 0: // Name
          if Name = '' then
            CellText := rsUnknown
          else
            CellText := Name;
        1: // Versión
          CellText := Format(rsNGames, [Node^.ChildCount]);
        2: // Developer / Publisher
          CellText := Developer;
        3: // Year
          CellText := Year;
        4: // Time played
          CellText := SecondToFmtStr(PlayingTime);
        5: // Times
          CellText := Format(rsNTimes, [TimesPlayed]);
        6: // Last time
          if LastTime = 0 then
            CellText := rsNever
          else
            CellText := DateTimeToStr(LastTime);
        7: // Filename
          CellText := MediaFileName;
        8: // Folder
          CellText := '';
      end;
    end;
  end
  else
  if Data^ is cGame then
  begin
    with cGame(Data^) do
    begin
      case Column of
        -1, 0: // Name
          if SameText(Name, SortKey) then
            CellText := Name
          else
            CellText := Name + ' (' + SortKey + ')';
        1: // Versión
          // TODO 5: HACK: If empty, the icons in OnDrawText are not drawn
          if Version = '' then
            CellText := ' '
          else
            CellText := Version;
        2: // Publisher
        begin
          CellText := Publisher;
          if Publisher = '' then
          begin
            TmpStr := cGameGroup(GameManager.Group(GameGroup)).Developer;
            if TmpStr <> '' then
              CellText := '(' + TmpStr + ')';
          end;
        end;
        3: // Year
        begin
          CellText := Year;
          if Year = '' then
          begin
            TmpStr := cGameGroup(GameManager.Group(GameGroup)).Year;
            if TmpStr <> '' then
              CellText := '(' + TmpStr + ')';
          end;
        end;
        4: // Time played
          CellText := SecondToFmtStr(PlayingTime);
        5: // Times
          CellText := Format(rsNTimes, [TimesPlayed]);
        6: // Last time
          if LastTime = 0 then
            CellText := rsNever
          else
            CellText := DateTimeToStr(LastTime);
        7: // Filename
          CellText := FileName;
        8: // Folder
          CellText := Folder;
      end;
    end;
  end;
end;

procedure TfrmGameManager.vstHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if Sender.SortColumn <> HitInfo.Column then
    Sender.SortDirection := sdAscending
  else
  begin
    if Sender.SortDirection = sdAscending then
      Sender.SortDirection := sdDescending
    else
      Sender.SortDirection := sdAscending;
  end;

  Sender.SortColumn := HitInfo.Column;

  Sender.Treeview.SortTree(Sender.SortColumn, Sender.SortDirection, True);
end;

procedure TfrmGameManager.SetConfig(const AValue: cConfig);
begin
  FConfig := AValue;
end;

procedure TfrmGameManager.SetCurrGame(const AValue: cGame);
begin
  FCurrGame := AValue;
end;

procedure TfrmGameManager.SetCurrGroup(const AValue: cGameGroup);
begin
  FCurrGroup := AValue;
end;

procedure TfrmGameManager.SetGameIcons(const AValue: cImageList);
begin
  FGameIcons := AValue;
end;

procedure TfrmGameManager.SetGameImagesIndex(const AValue: integer);
begin
  FGameImagesIndex := AValue;
end;

procedure TfrmGameManager.SetGameManager(const AValue: cGameManager);
begin
  FGameManager := AValue;
end;

procedure TfrmGameManager.SetGameTextsIndex(AValue: integer);
begin
  FGameTextsIndex := AValue;
end;

procedure TfrmGameManager.SetGroupIcons(const AValue: cImageList);
begin
  FGroupIcons := AValue;
end;

procedure TfrmGameManager.SetGroupList(const AValue: TFPObjectList);
begin
  FGroupList := AValue;
end;

procedure TfrmGameManager.SetGroupMode(const AValue: TlvGroupMode);
begin
  FGroupMode := AValue;
end;

procedure TfrmGameManager.SetrsAddingFile(const AValue: string);
begin
  FrsAddingFile := AValue;
end;

procedure TfrmGameManager.SetrsAssignToGroup(const AValue: string);
begin
  FrsAssignToGroup := AValue;
end;

procedure TfrmGameManager.SetrsChooseImageFileFormat(const AValue: string);
begin
  FrsChooseImageFileFormat := AValue;
end;

procedure TfrmGameManager.SetrsConfirmOverwriteFile(AValue: string);
begin
  FrsConfirmOverwriteFile := AValue;
end;

procedure TfrmGameManager.SetrsDecompressing(const AValue: string);
begin
  FrsDecompressing := AValue;
end;

procedure TfrmGameManager.SetrsDeveloper(const AValue: string);
begin
  FrsDeveloper := AValue;
end;

procedure TfrmGameManager.SetrsEmutecaGameDatabase(AValue: string);
begin
  FrsEmutecaGameDatabase := AValue;
end;

procedure TfrmGameManager.SetrsErrorEmulator(AValue: string);
begin
  FrsErrorEmulator := AValue;
end;

procedure TfrmGameManager.SetrsExportingData(const AValue: string);
begin
  FrsExportingData := AValue;
end;

procedure TfrmGameManager.SetrsFilename(const AValue: string);
begin
  FrsFilename := AValue;
end;

procedure TfrmGameManager.SetrsErrorGameNotFound(const AValue: string);
begin
  FrsErrorGameNotFound := AValue;
end;

procedure TfrmGameManager.SetrsImportingData(const AValue: string);
begin
  FrsImportingData := AValue;
end;

procedure TfrmGameManager.SetrsKey(const AValue: string);
begin
  FrsKey := AValue;
end;

procedure TfrmGameManager.SetrsLoadingGameList(const AValue: string);
begin
  FrsLoadingGameList := AValue;
end;

procedure TfrmGameManager.SetrsNever(const AValue: string);
begin
  FrsNever := AValue;
end;

procedure TfrmGameManager.SetrsNGames(const AValue: string);
begin
  FrsNGames := AValue;
end;

procedure TfrmGameManager.SetrsNGroups(const AValue: string);
begin
  FrsNGroups := AValue;
end;

procedure TfrmGameManager.SetrsNTimes(const AValue: string);
begin
  FrsNTimes := AValue;
end;

procedure TfrmGameManager.SetrsPublisher(const AValue: string);
begin
  FrsPublisher := AValue;
end;

procedure TfrmGameManager.SetrsPurgeMessage(AValue: string);
begin
  FrsPurgeMessage := AValue;
end;

procedure TfrmGameManager.SetrsSavingGameList(const AValue: string);
begin
  FrsSavingGameList := AValue;
end;

procedure TfrmGameManager.SetrsZones(const AValue: string);
begin
  FrsZones := AValue;
end;

procedure TfrmGameManager.SetrsUnknown(const AValue: string);
begin
  FrsUnknown := AValue;
end;

procedure TfrmGameManager.SetrsUpdatingList(const AValue: string);
begin
  FrsUpdatingList := AValue;
end;

procedure TfrmGameManager.SetrsVersion(const AValue: string);
begin
  FrsVersion := AValue;
end;

procedure TfrmGameManager.SetSystemIcons(const AValue: cImageList);
begin
  FSystemIcons := AValue;
end;

procedure TfrmGameManager.SetTempFolder(const AValue: string);
begin
  FTempFolder := IncludeTrailingPathDelimiter(AValue);
end;

function TfrmGameManager.Group(const aIndex: integer): cGameGroup;
begin
  if (aIndex < GroupCount) and (aIndex >= 0) then
    Result := cGameGroup(GroupList.Items[aIndex])
  else
    Result := nil;
end;

function TfrmGameManager.Group(aGameGroupID: string): cGameGroup;
var
  i: integer;
begin
  Result := nil;
  aGameGroupID := Trim(UTF8LowerCase(aGameGroupID));

  i := GroupCount - 1;
  while (i >= 0) and (Result = nil) do
  begin
    if Group(i).Key = aGameGroupID then
      Result := Group(i);
    Dec(i);
  end;
end;

function TfrmGameManager.GroupCount: longint;
begin
  Result := GroupList.Count;
end;

function TfrmGameManager.AddGroupVTV(aGameGroupID: string): PVirtualNode;
var
  aGroup: cGameGroup;
  PData: ^TObject;
  Nodo: PVirtualNode;
begin
  Result := nil;

  aGroup := Group(aGameGroupID);

  if aGroup = nil then
  begin // New group and node
    if GroupMode = lvGMGameGroup then // lvGMGameGroup
    begin
      aGroup := GameManager.Group(aGameGroupID);
      if aGroup = nil then
      begin
        // Some this is a UTF8/ANSI/ASCII mistake in somewhere...
        // The "¡" character is shown wrong :-O
        ShowMessage(UTF16ToUTF8(
          '¡WOOPS! Group not found in TfrmGameManager.AddGroupVTV.') +
          sLineBreak + aGameGroupID);
        Exit;
      end;
      // Adding Developer to ComboBox
      AddToStringList(cbPublisher.Items, aGroup.Developer);
    end
    else // Any other
      aGroup := cGameGroup.Create(aGameGroupID);

    GroupList.Add(aGroup);
    Result := vstGames.AddChild(nil);
    PData := vstGames.GetNodeData(Result);
    PData^ := aGroup;
  end
  else
  begin // We must search the node with the group
    Nodo := vstGames.GetLastChild(nil);
    while (Result = nil) and (Nodo <> nil) do
    begin
      PData := vstGames.GetNodeData(Nodo);
      if PData^ = aGroup then
        Result := Nodo;
      Nodo := vstGames.GetPreviousSibling(Nodo);
    end;
  end;
end;

procedure TfrmGameManager.UpdateSystemList;
var
  SysMan: cSystemManager;
  i: integer;
  aPos: integer;
begin
  cbSystem.Items.Clear;
  SystemIcons.Clear;

  SysMan := cSystemManager.Create(Config.DataFolder + Config.SystemsIniFile);
  try
    SysMan.ListEnabledSystems(cbSystem.Items);

    // Loading system icons
    // TODO 2: Don't load default images many times
    i := 0;
    while i < cbSystem.Items.Count do
    begin
      aPos := SystemIcons.AddImageFile(SysMan.System(cbSystem.Items[i]).Icon);
      if aPos = -1 then
      begin
        if FileExistsUTF8(Config.ImagesFolder + Config.DefaultImagesSubfolder +
          Config.DefaultSystemIcon) then
        begin
          aPos := SystemIcons.AddImageFile(Config.ImagesFolder +
            Config.DefaultImagesSubfolder + Config.DefaultSystemIcon);
          if aPos = -1 then
            SystemIcons.AddEmptyImage;
        end;
      end;
      Inc(i);
    end;
  finally
    FreeAndNil(SysMan);
  end;

  if cbSystem.Items.Count > 0 then
  begin
    cbSystem.ItemIndex := 0;
    ChangeCurrentSystem;
  end;
end;

procedure TfrmGameManager.UpdateSystemMediaCaptions;
begin
  cbGameImages.Items.Clear;
  cbGameTexts.Items.Clear;
  if GameManager.System = nil then
    Exit;

  cbGameImages.Items.AddStrings(GameManager.System.ImageCaptions);
  if cbGameImages.Items.Count > 0 then
    cbGameImages.ItemIndex := 0;

  cbGameTexts.Items.AddStrings(GameManager.System.TextCaptions);
  if cbGameTexts.Items.Count > 0 then
    cbGameTexts.ItemIndex := 0;
end;

procedure TfrmGameManager.UpdateSystemImage;
begin
  if GameManager.System = nil then
    Exit;

  if FileExistsUTF8(GameManager.System.Image) then
    ShowImage(GameManager.System.Image, iSystemImage)
  else
    ShowImage(Config.ImagesFolder + Config.DefaultImagesSubfolder +
      Config.DefaultSystemImage, iSystemImage);
end;

procedure TfrmGameManager.UpdateSystemBackground;
var
  aPicture: TPicture;
  aBitmap: TBitmap;
begin
  if GameManager.System = nil then
    Exit;
  if FileExistsUTF8(GameManager.System.BackgroundImage) then
  begin
    // Dirty workaround, VST only works well with TBitmap
    aPicture := TPicture.Create;
    aBitmap := TBitmap.Create;
    try
      aPicture.LoadFromFile(GameManager.System.BackgroundImage);
      aBitmap.Assign(aPicture.Bitmap);
      vstGames.Background.Assign(aBitmap);
    finally
      FreeAndNil(aBitmap);
      FreeAndNil(aPicture);
    end;
  end
  else
    vstGames.Background.Assign(nil);
end;

procedure TfrmGameManager.UpdateSystemText;
begin
  if GameManager.System = nil then
    Exit;
  ShowText(GameManager.System.InfoText, memoSystem);
end;

procedure TfrmGameManager.UpdateVTVGameList;
var
  i, j: integer;
  aGame: cGame;
  aGroup: cGameGroup;
  PData: ^TObject;
  Nodo: PVirtualNode;
  Continue: boolean;
begin
  Self.Enabled := False;

  vstGames.Clear;
  ClearGameMedia;
  CurrGame := nil;
  CurrGroup := nil;
  GameIcons.Clear;
  if FileExistsUTF8(Config.ImagesFolder + Config.DefaultImagesSubfolder +
    Config.DefaultGameIcon) then
    GameIcons.AddImageFile(Config.ImagesFolder + Config.DefaultImagesSubfolder +
      Config.DefaultGameIcon)
  else
    GameIcons.AddEmptyImage;

  if (GameManager.System = nil) then
    Exit;

  // Updating cbGameGroup list
  cbGameGroup.Items.Clear;
  i := GameManager.GroupCount - 1;
  cbGameGroup.Items.BeginUpdate;
  while i >= 0 do
  begin
    aGroup := GameManager.GroupAtPos(i);
    aGroup.IconIndex := -1;
    j := cbGameGroup.Items.Add(aGroup.Name);
    cbGameGroup.Items.Objects[j] := aGroup;
    Dec(i);
  end;
  cbGameGroup.Items.EndUpdate;

  // Cleaning da house &
  //   updating groups
  GroupList.Clear;
  // if GroupMode = lvGMGameGroup, when don't want to free the groups
  //   because they are owned by cGameManager
  GroupList.OwnsObjects := GroupMode <> lvGMGameGroup;

  vstGames.BeginUpdate;
  Application.CreateForm(TfrmProgress, frmProgress);
  try
    i := 0;
    Continue := True;
    while (i < GameManager.GameCount) and Continue do
    begin
      aGame := GameManager.GameAtPos(i);
      aGame.IconIndex := -1;
      Continue := frmProgress.UpdTextAndBar(rsUpdatingList,
        aGame.GameGroup, aGame.Name, i, GameManager.GameCount);

      // Adding Publisher to ComboBox
      AddToStringList(cbPublisher.Items, aGame.Publisher);

      Nodo := nil;
      case GroupMode of
        lvGMGameGroup: // AddGroupVTV take care about this
          Nodo := AddGroupVTV(aGame.GameGroup);
        lvGMName: Nodo := AddGroupVTV(aGame.Name);
        lvGMYear: Nodo := AddGroupVTV(LeftStr(aGame.Year, 4));
        lvGMDeveloper: Nodo :=
            AddGroupVTV(GameManager.Group(aGame.GameGroup).Developer);
        lvGMPublisher: Nodo := AddGroupVTV(aGame.Publisher);
        lvGMFolder: Nodo := AddGroupVTV(aGame.Folder);
        lvGMTags:
        begin
          // Slow and complex...
          // Adding tags from GameGroup
          aGroup := GameManager.Group(aGame.GameGroup);
          j := 0;
          while j < aGroup.Tags.Count do
          begin
            Nodo := AddGroupVTV(aGroup.Tags[j]);
            PData := vstGames.GetNodeData(vstGames.AddChild(Nodo));
            PData^ := aGame;
            Inc(j);
          end;
          // Adding tags from game itself
          j := 0;
          while j < aGame.Tags.Count do
          begin
            Nodo := AddGroupVTV(aGame.Tags[j]);
            PData := vstGames.GetNodeData(vstGames.AddChild(Nodo));
            PData^ := aGame;
            Inc(j);
          end;
          if (aGame.Tags.Count = 0) and (aGroup.Tags.Count = 0) then
            Nodo := AddGroupVTV('') // No tags, added to !Unknown by main loop
          else
            Nodo := nil;
        end;
      end;

      if Nodo <> nil then
      begin
        PData := vstGames.GetNodeData(vstGames.AddChild(Nodo));
        PData^ := aGame;
      end;
      Inc(i);
    end;

    // Adding playing time, times played and last time to groups
    Nodo := vstGames.GetLastChild(nil);
    while (Nodo <> nil) do
    begin
      UpdateGroupNodeTempData(Nodo);
      Nodo := vstGames.GetPreviousSibling(Nodo);
    end;

  finally
    vstGames.EndUpdate;
    FreeAndNil(frmProgress);
  end;

  sbInfo.Panels[0].Text := Format(rsNGroups, [GroupCount]);
  sbInfo.Panels[1].Text := Format(rsNGames, [GameManager.GameCount]);

  Self.Enabled := True;
  if Self.CanFocus then
    Self.SetFocus;
end;

procedure TfrmGameManager.UpdateGroupNodeTempData(Node: PVirtualNode);
var
  Data: ^TObject;
  aGroup: cGameGroup;
  aChildren: PVirtualNode;
begin
  if Node = nil then
    Exit;
  Data := vstGames.GetNodeData(Node);
  if not (Data^ is cGameGroup) then
    Exit;

  aGroup := cGameGroup(Data^);
  aGroup.LastTime := 0;
  aGroup.TimesPlayed := 0;
  aGroup.PlayingTime := 0;

  aChildren := vstGames.GetFirstChild(Node);
  while (aChildren <> nil) do
  begin
    Data := vstGames.GetNodeData(aChildren);

    if Data^ is cGame then
    begin
      with (Data^ as cGame) do
      begin
        if aGroup.LastTime < LastTime then
          aGroup.LastTime := LastTime;
        aGroup.TimesPlayed := aGroup.TimesPlayed + TimesPlayed;
        aGroup.PlayingTime := aGroup.PlayingTime + PlayingTime;
      end;
    end;
    aChildren := vstGames.GetNextSibling(aChildren);
  end;
end;

procedure TfrmGameManager.UpdateSystemMedia;
begin
  UpdateSystemImage;
  UpdateSystemBackground;
  UpdateSystemText;
  UpdateSystemMediaCaptions;
  UpdateVTVGameList;
end;

procedure TfrmGameManager.UpdateGameMedia;
begin
  ClearGameMedia;
  UpdateGameImage;
  UpdateGameText;
  UpdateGameEditFields;
  UpdateGameProperties;
end;

procedure TfrmGameManager.UpdateGameProperties;
begin
  if CurrGame = nil then
    Exit;

  pagProperties.Enabled := True;

  eAlternate.Text := CurrGame.Alternate;
  chkVerified.Checked := CurrGame.Verified;
  eBadDump.Text := CurrGame.BadDump;
  ePirate.Text := CurrGame.Pirate;
  eLicense.Text := CurrGame.License;
  eLanguages.Text := CurrGame.Languages.CommaText;
  eTranslation.Text := CurrGame.Translation;
  eCracked.Text := CurrGame.Cracked;
  eFixed.Text := CurrGame.Fixed;
  eModified.Text := CurrGame.Modified;
  eTrainer.Text := CurrGame.Trainer;
  eHack.Text := CurrGame.Hack;
end;

procedure TfrmGameManager.UpdateGroupMedia;
begin
  ClearGameMedia;
  UpdateGroupImage;
  UpdateGroupText;
  UpdateGroupEditFields;
  ClearProperties;
  pagProperties.Enabled := False;
end;

procedure TfrmGameManager.ChangeCurrentSystem;
begin
  Self.Enabled := False;
  Application.CreateForm(TfrmProgress, frmProgress);
  try
    GameManager.ChangeSystem(cbSystem.Text);
  finally
    FreeAndNil(frmProgress);
  end;
  Self.Enabled := True;

  UpdateSystemMedia;
  UpdateGameMedia;
  UpdateEmulatorList;
  FillOtherFilesTree;

  if Self.CanFocus then
    Self.SetFocus;
end;

procedure TfrmGameManager.UpdateGameImage;
begin
  GameImages.Clear;
  actPreviousGameImage.Enabled := False;
  actNextGameImage.Enabled := False;

  if CurrGame <> nil then
    SearchGameImage(CurrGame);

  if GameImages.Count > 1 then
  begin
    actPreviousGameImage.Enabled := True;
    actNextGameImage.Enabled := True;
  end;

  GameImagesIndex := 0;
  if GameImages.Count > 0 then
    ShowImage(GameImages[GameImagesIndex], iGameImage)
  else
    ShowImage(Config.ImagesFolder + Config.DefaultImagesSubfolder +
      Config.DefaultGameImage, iGameImage);
end;

procedure TfrmGameManager.NextGameImage;
begin
  if GameImages.Count <= 1 then
    Exit;

  Inc(FGameImagesIndex);
  if GameImagesIndex >= GameImages.Count then
    GameImagesIndex := 0;

  ShowImage(GameImages[GameImagesIndex], iGameImage);
end;

procedure TfrmGameManager.PreviousGameImage;
begin
  if GameImages.Count <= 1 then
    Exit;

  Dec(FGameImagesIndex);
  if GameImagesIndex < 0 then
    GameImagesIndex := GameImages.Count - 1;

  ShowImage(GameImages[GameImagesIndex], iGameImage);
end;

procedure TfrmGameManager.PasteGameImage;
var
  aFileName: string;
begin
  if (CurrGroup = nil) or (cbGameImages.ItemIndex = -1) then
    Exit;

  LoadImageFromClipboard(iGameImage.Picture);

  aFilename := CurrGroup.MediaFileName;

  // Check if can be assigned to game
  if (CurrGame <> nil) then
    if MessageDlg(rsAssignToGroup, mtConfirmation, [mbYes, mbNo], 0) = mrNo then
      // TODO 1: Avisar que el juego no puede tener su propia imagen si se
      //   llama igual que el grupo...
      aFilename := RemoveFromBrackets(CurrGame.FileName) + CVirtualGameExt;

  if StrToBoolDef(GameManager.System.ImageModes[cbGameImages.ItemIndex], False) then
  begin // Folders mode
    aFilename := SetAsFolder(GameManager.System.ImageFolders[
      cbGameImages.ItemIndex] + ExtractFileNameOnly(aFilename));
    ForceDirectoriesUTF8(aFilename);
    // We need a unique filename...
    aFilename := aFilename + FormatDateTime('yyyymmddhhnn', Now) + CVirtualGameExt;
  end
  else
  begin
    aFilename := GameManager.System.ImageFolders[cbGameImages.ItemIndex] +
      aFileName;
  end;

  SaveImageToFile(iGameImage.Picture, aFileName);
end;

procedure TfrmGameManager.PasteGameSpineImage;
var
  aFileName: string;
  aPicture: TPicture;
begin
  if (CurrGroup = nil) or (cbGameImages.ItemIndex = -1) then
    Exit;

  aPicture := TPicture.Create;
  try
    // Loading from clipboard
    LoadImageFromClipboard(aPicture);

    aFilename := CurrGroup.MediaFileName;
    // Check if it must be assigned to the game
    if (CurrGame <> nil) then
      if MessageDlg(rsAssignToGroup, mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        aFilename := RemoveFromBrackets(CurrGame.FileName) + CVirtualGameExt;

    aFilename := GameManager.System.MarqueeFolder + aFileName;

    SaveImageToFile(aPicture, aFileName);
  finally
    FreeAndNil(aPicture);
  end;
end;

procedure TfrmGameManager.PasteGameIconImage;
var
  aFileName: string;
  aPicture: TPicture;
begin
  if (CurrGroup = nil) or (cbGameImages.ItemIndex = -1) then
    Exit;

  aPicture := TPicture.Create;
  try
    // Loading from clipboard
    LoadImageFromClipboard(aPicture);

    aFilename := CurrGroup.MediaFileName;
    // Check if it must be assigned to the game
    if (CurrGame <> nil) then
      if MessageDlg(rsAssignToGroup, mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        aFilename := RemoveFromBrackets(CurrGame.FileName) + CVirtualGameExt;

    aFilename := GameManager.System.IconFolder + aFileName;

    SaveImageToFile(aPicture, aFileName);
  finally
    FreeAndNil(aPicture);
  end;

  // TODO 2: Actualizar icono en la lista...
end;

procedure TfrmGameManager.SaveGameText;
var
  aFileName: string;
begin
  if (CurrGroup = nil) or (cbGameTexts.ItemIndex = -1) then
    Exit;

  // TODO 2: We must diferenciate between adding a new file
  //   o overwrite existing one in folders mode.
  //   By now, Emuteca always overwrite existing file.

  if (GameTexts.Count <> 0) and
    // Test if it is not in a compressed archive
    (UTF8Pos(TempFolder, GameTexts[GameTextsIndex]) = 0) then
  begin // File already exists
    aFileName := GameTexts[GameTextsIndex];
  end
  else
  begin // New file or it's in compressed archive
    aFilename := CurrGroup.MediaFileName;
    if (CurrGame <> nil) then
    begin
      if MessageDlg(rsAssignToGroup, mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        // Whoops, only for the game
        aFilename := RemoveFromBrackets(CurrGame.FileName) + CVirtualGameExt;
    end;

    if StrToBoolDef(GameManager.System.TextModes[cbGameTexts.ItemIndex], False) then
    begin // Folders mode
      aFilename := SetAsFolder(
        GameManager.System.TextFolders[cbGameTexts.ItemIndex] +
        ExtractFileNameOnly(aFilename));
      ForceDirectoriesUTF8(aFilename);
      // We need a unique filename...
      aFilename := aFilename + FormatDateTime('yyyymmddhhnn', Now) +
        CVirtualGameExt;
    end
    else
    begin
      aFilename := GameManager.System.TextFolders[cbGameTexts.ItemIndex] +
        aFileName;
    end;
  end;
  SaveTextToFile(memoGame.Lines, aFileName);
end;

procedure TfrmGameManager.SaveImageToFile(aPicture: TPicture; aFilename: string);
var
  Extension: string;
begin
  if aPicture = nil then
    Exit;
  if ExtractFileNameOnly(aFilename) = '' then
    Exit;
  if ExtractFileExt(aFilename) = '' then
    aFilename := aFilename + ExtensionSeparator + 'ext';

  if MessageDlg(rsChooseImageFileFormat, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    Extension := '.png'
  else
    Extension := '.jpg';

  aFilename := ChangeFileExt(aFilename, Extension);

  if FileExistsUTF8(aFilename) then
    if MessageDlg(Format(rsConfirmOverwriteFile, [aFilename]),
      mtConfirmation, [mbYes, mbNo], 0) = mrNo then
      Exit;

  aPicture.SaveToFile(aFilename);
end;

procedure TfrmGameManager.SaveTextToFile(aText: TStrings; aFilename: string);
begin
  if aText = nil then
    Exit;
  if aFilename = '' then
    Exit;
  if ExtractFileExt(aFilename) = '' then
    aFilename := aFilename + ExtensionSeparator + 'txt';

  aFilename := ChangeFileExt(aFilename, '.txt');

  if FileExistsUTF8(aFilename) then
    if MessageDlg(Format(rsConfirmOverwriteFile, [aFilename]),
      mtConfirmation, [mbYes, mbNo], 0) = mrNo then
      Exit;

  aText.SaveToFile(UTF8ToSys(aFilename));
end;

procedure TfrmGameManager.LoadImageFromClipboard(aPicture: TPicture);
var
  CF: TClipboardFormat;
begin
  if aPicture = nil then
    Exit;

  // Loading from clipboard
  CF := Clipboard.FindPictureFormatID;
  if CF = Windows.CF_BITMAP then // Handle CF_BITMAP separately
    aPicture.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfDelphiBitmap))
  else
    aPicture.LoadFromClipboardFormat(CF);
end;

procedure TfrmGameManager.FillOtherFilesTree;
var
  aNode: PVirtualNode;
  PData: ^string;
  i: integer;
begin
  vstFiles.Clear;

{
  vstFiles.BeginUpdate;

  // TODO 1: Make localizable
  // TODO 1: FIXME: If Other Files tab is not opened
  aNode := vstFiles.AddChild(nil);
  PData :=  vstFiles.GetNodeData(aNode);
  PData^ := 'Images';
  if GameManager.System <> nil then
  begin
    for i := 0 to GameManager.System.ImageFolders.Count - 1 do
    begin
      PData := vstFiles.GetNodeData(vstFiles.AddChild(aNode));
      PData^ := GameManager.System.ImageCaptions[i];
    end;
  end;

  aNode := vstFiles.AddChild(nil);
  PData :=  vstFiles.GetNodeData(aNode);
  PData^ := 'Texts';
  if GameManager.System <> nil then
  begin
    for i := 0 to GameManager.System.TextFolders.Count - 1 do
    begin
      PData := vstFiles.GetNodeData(vstFiles.AddChild(aNode));
      PData^ := GameManager.System.TextCaptions[i];
    end;
  end;

  aNode := vstFiles.AddChild(nil);
  PData := vstFiles.GetNodeData(aNode);
  PData^ := 'Music';
  if GameManager.System <> nil then
  begin
    for i := 0 to GameManager.System.MusicFolders.Count - 1 do
    begin
      PData := vstFiles.GetNodeData(vstFiles.AddChild(aNode));
      PData^ := GameManager.System.MusicCaptions[i];
    end;
  end;

  aNode := vstFiles.AddChild(nil);
  PData := vstFiles.GetNodeData(aNode);
  PData^ := 'Videos';
  if GameManager.System <> nil then
  begin
    for i := 0 to GameManager.System.VideoFolders.Count - 1 do
    begin
      PData := vstFiles.GetNodeData(vstFiles.AddChild(aNode));
      PData^ := GameManager.System.VideoCaptions[i];
    end;
  end;

  aNode := vstFiles.AddChild(nil);
  PData :=  vstFiles.GetNodeData(aNode);
  PData^ := 'Other';

  // TODO 2: Add other files folders.

  vstFiles.EndUpdate;
}
end;

procedure TfrmGameManager.UpdateGameText;
begin
  GameTexts.Clear;
  actPreviousGameText.Enabled := False;
  actNextGameText.Enabled := False;

  if CurrGame <> nil then
    SearchGameText(CurrGame);

  if GameTexts.Count > 1 then
  begin
    actPreviousGameText.Enabled := True;
    actNextGameText.Enabled := True;
  end;

  GameTextsIndex := 0;
  if GameTexts.Count > 0 then
    ShowText(GameTexts[GameTextsIndex], memoGame)
  else
    ShowText('', memoGame);
end;

procedure TfrmGameManager.UpdateGroupImage;
begin
  GameImages.Clear;
  actPreviousGameImage.Enabled := False;
  actNextGameImage.Enabled := False;

  if CurrGroup <> nil then
    SearchGroupImage(GameImages, CurrGroup);

  if GameImages.Count > 1 then
  begin
    actPreviousGameImage.Enabled := True;
    actNextGameImage.Enabled := True;
  end;

  GameImagesIndex := 0;
  if GameImages.Count > 0 then
    ShowImage(GameImages[GameImagesIndex], iGameImage)
  else
    ShowImage(Config.ImagesFolder + Config.DefaultImagesSubfolder +
      Config.DefaultGameImage, iGameImage);
end;

procedure TfrmGameManager.UpdateGroupText;
begin
  GameTexts.Clear;
  actPreviousGameText.Enabled := False;
  actNextGameText.Enabled := False;

  if CurrGroup <> nil then
    SearchGroupText(GameTexts, CurrGroup);

  if GameTexts.Count > 1 then
  begin
    actPreviousGameText.Enabled := True;
    actNextGameText.Enabled := True;
  end;

  GameTextsIndex := 0;
  if GameTexts.Count > 0 then
    ShowText(GameTexts[GameTextsIndex], memoGame)
  else
    ShowText('', memoGame);
end;

procedure TfrmGameManager.UpdateGroupEditFields;
begin
  if CurrGroup = nil then
    Exit;

  lName.Enabled := True;
  eName.Enabled := True;
  eName.Caption := CurrGroup.Name;

  lSortKey.Enabled := True;
  eSortKey.Enabled := True;
  eSortKey.Text := CurrGroup.SortKey;

  lPublisher.Enabled := True;
  lPublisher.Caption := rsDeveloper;
  cbPublisher.Enabled := True;
  cbPublisher.Caption := CurrGroup.Developer;

  // Reusing Zone edit to show group key
  lZones.Enabled := False;
  lZones.Caption := rsKey;
  eZones.Enabled := False;
  eZones.Text := CurrGroup.Key;

  cbYear.Enabled := True;
  cbYear.Caption := CurrGroup.Year;

  lVersion.Enabled := True;
  lVersion.Caption := rsFilename;
  eVersion.Enabled := True;
  eVersion.Caption := ExtractFileNameWithoutExt(CurrGroup.MediaFileName);

  lGameGroup.Enabled := False;
  cbGameGroup.Enabled := False;

  lGroupTags.Enabled := True;
  mmGroupTags.Enabled := True;
  mmGroupTags.Lines.AddStrings(CurrGroup.Tags);
  lGameTags.Enabled := False;
  lGameTags.Visible := False;
  mmGameTags.Enabled := False;
  mmGameTags.Visible := False;

  sbInfo.Panels[3].Text := ExtractFileNameWithoutExt(CurrGroup.MediaFileName);
end;

procedure TfrmGameManager.SaveGroupData;
var
  UpdateVTV: boolean;
begin
  case GroupMode of
    lvGMGameGroup:
    begin
      UpdateVTV := False;
      CurrGroup.Name := eName.Text;

      // TODO 1: We can't change/merge groups and their keys...

      CurrGroup.SortKey := eSortKey.Text;
      CurrGroup.Year := cbYear.Text;
      CurrGroup.Developer := cbPublisher.Text;
      AddToStringList(cbPublisher.Items, CurrGroup.Developer);
      CurrGroup.MediaFileName := eVersion.Text;

      if mmGroupTags.Modified then
      begin
        CurrGroup.Tags.Clear;
        CurrGroup.Tags.AddStrings(mmGroupTags.Lines);
        UpdateVTV := GroupMode = lvGMTags;
      end;

      if UpdateVTV then
        UpdateVTVGameList
      else
        vstGames.Refresh;
    end;
  end;
end;

procedure TfrmGameManager.UpdateEmulatorList;
begin
  if GameManager.System = nil then
    Exit;

  sbInfo.Panels[2].Text := '';
  cbEmulators.Clear;
  cbEmulators.Items.AddStrings(GameManager.System.OtherEmulators);

  if GameManager.Emulator = nil then
    Exit;
  cbEmulators.ItemIndex := AddToStringList(cbEmulators.Items,
    GameManager.Emulator.ID);

  UpdateEmulatorMedia;
end;

procedure TfrmGameManager.ChangeCurrentEmulator;
begin
  GameManager.ChangeEmulator(cbEmulators.Items[cbEmulators.ItemIndex]);
  UpdateEmulatorMedia;
end;

procedure TfrmGameManager.UpdateEmulatorMedia;
begin
  sbInfo.Panels[2].Text := '';

  UpdateEmulatorImage;
  UpdateEmulatorText;

  if GameManager.Emulator = nil then
    Exit;
  sbInfo.Panels[2].Text := GameManager.Emulator.Name;
end;

procedure TfrmGameManager.UpdateEmulatorImage;
begin
  iEmulatorImage.Picture.Clear;

  if GameManager.Emulator = nil then
    Exit;

  if FileExistsUTF8(GameManager.Emulator.Image) then
  begin
    iEmulatorImage.Picture.LoadFromFile(GameManager.Emulator.Image);
  end
  else
  // Uhm, no emulator image
  if FileExistsUTF8(Config.ImagesFolder + Config.DefaultImagesSubfolder +
    Config.DefaultEmulatorImage) then
    iEmulatorImage.Picture.LoadFromFile(Config.ImagesFolder +
      Config.DefaultImagesSubfolder + Config.DefaultEmulatorImage);
end;

procedure TfrmGameManager.UpdateEmulatorText;
begin
  memoEmulator.Clear;

  if GameManager.Emulator = nil then
    Exit;
  if not FileExistsUTF8(GameManager.Emulator.InfoFile) then
    Exit;

  memoEmulator.Lines.LoadFromFile(GameManager.Emulator.InfoFile);
end;

procedure TfrmGameManager.ClearGameMedia;
begin
  eName.Caption := '';
  eSortKey.Caption := '';
  eZones.Caption := '';
  cbYear.Caption := '';
  eVersion.Caption := '';
  cbPublisher.Caption := '';
  cbGameGroup.Caption := '';
  mmGameTags.Clear;
  mmGroupTags.Clear;

  iGameImage.Picture.Clear;
  memoGame.Clear;
end;

procedure TfrmGameManager.ClearProperties;
begin
  chkVerified.Checked := False;
end;

procedure TfrmGameManager.SearchInInternet(Sender: TObject);
var
  aAction: TCustomAction;
  TempStr: string;
begin
  if not (Sender is TCustomAction) then
    Exit;
  aAction := TCustomAction(Sender);

  case aAction.Tag of
    1:
    begin
      if CurrGroup = nil then
        Exit;
      if CurrGame <> nil then
        TempStr := CurrGame.Name
      else
        TempStr := CurrGroup.Name;
    end;
    2:
    begin
      if GameManager.System = nil then
        Exit;
      TempStr := GameManager.System.Company + ' ' + GameManager.System.Model;
    end;
    3:
    begin
      if GameManager.Emulator = nil then
        Exit;
      TempStr := GameManager.Emulator.Name;
    end;
    else
    begin
      Exit;
    end;
  end;
  if TempStr = '' then
    Exit;

  TempStr := Format(aAction.Hint, [TempStr]);
  OpenURL(TempStr);
end;

procedure TfrmGameManager.SearchGroupText(StrList: TStrings; aGameGroup: cGameGroup);
begin
  if aGameGroup = nil then
    Exit;
  case GroupMode of
    // Aprovechamos las ventajas del cGameManager :P
    lvGMYear: GameManager.SearchGroupMedia(StrList,
        Config.CommonMediaFolder + SetAsFolder('Years') + SetAsFolder('Texts'),
        aGameGroup, Config.TextExtensions, False, True);
    lvGMDeveloper: GameManager.SearchGroupMedia(StrList,
        Config.CommonMediaFolder + SetAsFolder('Companies') + SetAsFolder('Texts'),
        aGameGroup, Config.TextExtensions, False, True);
    lvGMPublisher: GameManager.SearchGroupMedia(StrList,
        Config.CommonMediaFolder + SetAsFolder('Companies') + SetAsFolder('Texts'),
        aGameGroup, Config.TextExtensions, False, True);
    { In folder mode... search in default location...
    lvGMFolder: GameManager.SearchGroupMedia(StrList,
        Config.CommonMediaFolder + SetAsFolder('Folders') + SetAsFolder('Texts'),
        aGameGroup, Config.TextExtensions, False, True);
    }
    lvGMTags: GameManager.SearchGroupMedia(StrList,
        Config.CommonMediaFolder + SetAsFolder('Tags') + SetAsFolder('Texts'),
        aGameGroup, Config.TextExtensions, False, True);
    else
      if cbGameTexts.ItemIndex > -1 then
        GameManager.SearchGroupMedia(StrList,
          GameManager.System.TextFolders[cbGameTexts.ItemIndex], aGameGroup,
          Config.TextExtensions,
          StrToBoolDef(GameManager.System.TextModes[cbGameImages.ItemIndex],
          False), True);
  end;
end;

procedure TfrmGameManager.SearchGroupImage(StrList: TStrings; aGameGroup: cGameGroup);
begin
  if aGameGroup = nil then
    Exit;

  case GroupMode of
    // Aprovechamos las ventajas del cGameManager :P
    lvGMYear: GameManager.SearchGroupMedia(StrList,
        Config.CommonMediaFolder + SetAsFolder('Years') + SetAsFolder('Images'),
        aGameGroup, Config.ImageExtensions, False, True);
    lvGMDeveloper: GameManager.SearchGroupMedia(StrList,
        Config.CommonMediaFolder + SetAsFolder('Companies') +
        SetAsFolder('Images'), aGameGroup, Config.ImageExtensions,
        False, True);
    lvGMPublisher: GameManager.SearchGroupMedia(StrList,
        Config.CommonMediaFolder + SetAsFolder('Companies') +
        SetAsFolder('Images'), aGameGroup, Config.ImageExtensions,
        False, True);
    { In folder mode... search in default location...

     lvGMFolder: GameManager.SearchGroupMedia(StrList,
        Config.CommonMediaFolder + SetAsFolder('Folders' + PathDelim +
        'Images' + PathDelim, aGameGroup, Config.ImageExtensions, False, True);

    }
    lvGMTags: GameManager.SearchGroupMedia(StrList,
        Config.CommonMediaFolder + SetAsFolder('Tags') + SetAsFolder('Images'),
        aGameGroup, Config.ImageExtensions, False, True);
    else  // By default, standard search
      if cbGameImages.ItemIndex > -1 then
        GameManager.SearchGroupMedia(StrList,
          GameManager.System.ImageFolders[cbGameImages.ItemIndex],
          aGameGroup, Config.ImageExtensions,
          StrToBoolDef(GameManager.System.ImageModes[cbGameImages.ItemIndex],
          False), True);
  end;
end;

procedure TfrmGameManager.SearchGameText(aGame: cGame);
begin
  GameTexts.Clear;
  if cbGameTexts.ItemIndex > -1 then
    GameManager.SearchGameMedia(GameTexts,
      GameManager.System.TextFolders[cbGameTexts.ItemIndex], aGame,
      Config.TextExtensions, StrToBoolDef(
      GameManager.System.TextModes[cbGameTexts.ItemIndex], False), True);
end;

procedure TfrmGameManager.SearchGameImage(aGame: cGame);
begin
  GameImages.Clear;
  if cbGameImages.ItemIndex > -1 then
    GameManager.SearchGameMedia(GameImages,
      GameManager.System.ImageFolders[cbGameImages.ItemIndex], aGame,
      Config.ImageExtensions, StrToBoolDef(
      GameManager.System.ImageModes[cbGameImages.ItemIndex], False), True);
end;

procedure TfrmGameManager.SearchGames;
begin
  vstGames.BeginUpdate;
  try
    // Show all nodes
    vstGames.IterateSubtree(nil, @ShowAllNodes, nil);
    // Hide non matching nodes
    if eSearch.Text <> '' then
      vstGames.IterateSubtree(nil, @HideNodes, nil);
  finally
    vstGames.EndUpdate;
  end;
end;

procedure TfrmGameManager.ShowAllNodes(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: boolean);
begin
  Abort := False;
  Sender.IsVisible[Node] := True;
  Sender.Expanded[Node^.Parent] := False;
end;

procedure TfrmGameManager.HideNodes(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: boolean);
var
  NodeData: ^TObject;
  aGame: cGame;
  Temp: string;
begin
  Abort := False;
  if eSearch.Text = '' then
    Exit;

  NodeData := vstGames.GetNodeData(Node);

  // Hidding groups
  if not (NodeData^ is cGame) then
  begin
    Sender.IsVisible[Node] := False;
    Exit;
  end;

  aGame := cGame(NodeData^);
  // Some cases seems to be redundant with list type...
  //   but with GroupMode can be useful.
  case cbSearch.ItemIndex of
    1: Temp := GameManager.Group(aGame.GameGroup).Developer;
    2: Temp := aGame.Publisher;
    3: Temp := aGame.Year;
    4: Temp := aGame.Tags.CommaText;
    5: Temp := aGame.Tags.CommaText + GameManager.Group(
        aGame.GameGroup).Tags.CommaText;
    6: Temp := aGame.Version
    else
      Temp := aGame.Name;
  end;

  Temp := UTF8LowerCase(Temp);
  if UTF8Pos(UTF8LowerCase(eSearch.Text), Temp) = 0 then
    Sender.IsVisible[Node] := False
  else
  begin
    // Shows parent (game group) node
    if (Sender.IsVisible[Node^.Parent] = False) then
    begin
      Sender.IsVisible[Node^.Parent] := True;
    end;
  end;
end;

procedure TfrmGameManager.UpdateGameEditFields;
begin
  if CurrGame = nil then
    Exit;

  lName.Enabled := True;
  eName.Enabled := True;
  eName.Text := CurrGame.Name;

  lSortKey.Enabled := True;
  eSortKey.Enabled := True;
  eSortKey.Text := CurrGame.SortKey;

  lPublisher.Enabled := True;
  lPublisher.Caption := rsPublisher;
  cbPublisher.Enabled := True;
  cbPublisher.Text := CurrGame.Publisher;

  lYear.Enabled := True;
  cbYear.Enabled := True;
  cbYear.Text := CurrGame.Year;

  lZones.Enabled := True;
  lZones.Caption := rsZones;
  eZones.Enabled := True;
  eZones.Text := CurrGame.Zones.CommaText;

  lVersion.Caption := rsVersion;
  eVersion.Enabled := True;
  eVersion.Text := CurrGame.Version;

  lGameGroup.Enabled := True;
  cbGameGroup.Enabled := True;
  cbGameGroup.Text := GameManager.Group(CurrGame.GameGroup).Name;

  lGroupTags.Enabled := True;
  mmGroupTags.Enabled := True;
  mmGroupTags.Lines.AddStrings(CurrGroup.Tags);
  mmGroupTags.Modified := False;
  lGameTags.Enabled := True;
  lGameTags.Visible := True;
  mmGameTags.Enabled := True;
  mmGameTags.Visible := True;
  mmGameTags.Lines.AddStrings(CurrGame.Tags);
  mmGameTags.Modified := False;

  sbInfo.Panels[3].Text := CurrGame.Folder + CurrGame.FileName;
end;

procedure TfrmGameManager.SaveGameData;
var
  UpdateVTV: boolean;
  aGameGroup: cGameGroup;
begin
  if CurrGame = nil then
    Exit;

  UpdateVTV := False;

  if (eName.Text <> '') and (eName.Text <> CurrGame.Name) then
  begin
    CurrGame.Name := eName.Text;
    UpdateVTV := GroupMode = lvGMName;
  end;

  if (eSortKey.Text <> '') and (eSortKey.Text <> CurrGame.SortKey) then
    CurrGame.SortKey := eSortKey.Text;

  CurrGame.Zones.CommaText := eZones.Text;

  if cbYear.Text <> CurrGame.Year then
  begin
    CurrGame.Year := cbYear.Text;
    UpdateVTV := GroupMode = lvGMYear;
  end;

  CurrGame.Version := eVersion.Text;

  if cbPublisher.Text <> CurrGame.Publisher then
  begin
    CurrGame.Publisher := cbPublisher.Text;
    AddToStringList(cbPublisher.Items, CurrGame.Publisher);
    UpdateVTV := GroupMode = lvGMPublisher;
  end;

  // TODO 1: We can't create new groups from editor
  if (cbGameGroup.Text <> '') and (cbGameGroup.Text <> CurrGroup.Name) then
  begin
    if cbGameGroup.ItemIndex <> -1 then
    begin
      aGameGroup := cGameGroup(cbGameGroup.Items.Objects[cbGameGroup.ItemIndex]);
      CurrGame.GameGroup := aGameGroup.Key;
      UpdateVTV := GroupMode = lvGMGameGroup;
    end;
  end;

  if mmGroupTags.Modified then
  begin
    // CurrGroup is the old group, and it can be changed in the previous field
    //   so we need get it from CurrGame. (If all goes well, mmGroupTags is
    //   updated when selecting the new group).
    aGameGroup := GameManager.Group(CurrGame.GameGroup);
    aGameGroup.Tags.Clear;
    aGameGroup.Tags.AddStrings(mmGroupTags.Lines);
    UpdateVTV := GroupMode = lvGMTags;
  end;

  if mmGameTags.Modified then
  begin
    CurrGame.Tags.Clear;
    CurrGame.Tags.AddStrings(mmGameTags.Lines);
    UpdateVTV := GroupMode = lvGMTags;
  end;

  if UpdateVTV then
    UpdateVTVGameList
  else
    vstGames.Refresh;
end;

procedure TfrmGameManager.OpenGameImages;
var
  FormIV: TfrmImageViewer;
  // FormResult: TModalResult;
begin
  if GameImages.Count = 0 then
    Exit;
  Application.CreateForm(TfrmImageViewer, FormIV);
  try
    FormIV.Config := Self.Config;
    FormIV.AddImages(GameImages, GameImagesIndex);
    { FormResult :=} FormIV.ShowModal;
  finally
    FreeAndNil(FormIV);
  end;
end;

procedure TfrmGameManager.OpenSystemImages;
var
  FormIV: TfrmImageViewer;
  // FormResult: TModalResult;
begin
  if not FileExistsUTF8(GameManager.System.Image) then
    Exit;
  Application.CreateForm(TfrmImageViewer, FormIV);
  try
    FormIV.Config := Self.Config;
    FormIV.AddImage(GameManager.System.Image);
    { FormResult :=} FormIV.ShowModal;
  finally
    FreeAndNil(FormIV);
  end;
end;

procedure TfrmGameManager.OpenSystemFolder;
var
  SystemFolder: string;
begin
  if GameManager.System = nil then
    Exit;

  SystemFolder := GameManager.System.BaseFolder;

  // If system base folder don't exists, then
  //   try to open games folder parent.
  if not DirectoryExistsUTF8(SystemFolder) then
    SystemFolder := ExtractFilePath(ExcludeTrailingPathDelimiter(
      GameManager.System.GameFolder));

  if not DirectoryExistsUTF8(SystemFolder) then
    Exit;

  OpenDocument(SystemFolder);
end;

procedure TfrmGameManager.OpenEmulatorImages;
var
  FormIV: TfrmImageViewer;
begin
  if not FileExistsUTF8(GameManager.Emulator.Image) then
    Exit;
  Application.CreateForm(TfrmImageViewer, FormIV);
  try
    FormIV.Config := Self.Config;
    FormIV.AddImage(GameManager.Emulator.Image);
    FormIV.ShowModal;
  finally
    FreeAndNil(FormIV);
  end;
end;

procedure TfrmGameManager.OpenEmulatorFolder;
var
  EmulatorFolder: string;
begin
  if GameManager.Emulator = nil then
    Exit;

  EmulatorFolder := ExtractFilePath(GameManager.Emulator.ExeFile);

  if not DirectoryExistsUTF8(EmulatorFolder) then
    Exit;

  OpenDocument(EmulatorFolder);
end;

procedure TfrmGameManager.ShowImage(aFileName: string; aViewer: TImage);
begin
  if FileExistsUTF8(aFileName) then
    aViewer.Picture.LoadFromFile(aFileName)
  else
    aViewer.Picture.Clear;

  // Meh, I don't like this code here...
  lImageCount.Caption := IntToStr(GameImagesIndex + 1) + ' / ' +
    IntToStr(GameImages.Count);
end;

procedure TfrmGameManager.ShowText(aFileName: string; aViewer: TMemo);
begin
  if FileExistsUTF8(aFileName) then
    aViewer.Lines.LoadFromFile(UTF8ToSys(aFileName))
  else
    aViewer.Clear;

  // Meh, I don't like this code here...
  lTextCount.Caption := IntToStr(GameTextsIndex + 1) + ' / ' +
    IntToStr(GameTexts.Count);
end;

function TfrmGameManager.AddZoneIcon(Folder: string; Info: TSearchRec): boolean;
var
  aString: string;
begin
  Result := True;
  aString := UTF8LowerCase(UTF8Copy(ExtractFileExt(Info.Name), 2, MaxInt));
  // Is it an image?
  if Config.ImageExtensions.IndexOf(aString) = -1 then
    Exit;

  aString := UTF8LowerCase(ExtractFileNameOnly(Info.Name));
  // Is it already added the zone?
  if ZoneList.IndexOf(aString) <> -1 then
    Exit;

  // Adding the icon
  ZoneList.Add(aString);
  ZoneIcons.AddImageFile(SetAsFolder(Folder) + Info.Name);
end;

function TfrmGameManager.GMProgressCall(const TypeCB: TGMCallBackType;
  const Info1, Info2: string; const Value, Max: int64): boolean;
var
  aAction: string;
begin
  Result := True; // True -> Don't abort

  if frmProgress = nil then
    Exit;

  case TypeCB of
    GMCBAddFile: aAction := rsAddingFile;
    GMCBImportData: aAction := rsImportingData;
    GMCBExportData: aAction := rsExportingData;
    GMCBSaveList: aAction := rsSavingGameList;
    GMCBLoadList: aAction := rsLoadingGameList;
    GMCBDecompress: aAction := rsDecompressing;
  end;

  Result := frmProgress.UpdTextAndBar(aAction, Info1, Info2, Value, Max);
end;

procedure TfrmGameManager.actExitExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmGameManager.actExportSystemDataExecute(Sender: TObject);
begin
  SaveDialog.Filter := rsEmutecaGameDatabase + CDBFilter;
  SaveDialog.DefaultExt := CDBExt;
  if not SaveDialog.Execute then
    Exit;
  Application.CreateForm(TfrmProgress, frmProgress);
  try
    GameManager.ExportGameData(SaveDialog.FileName, True);
  finally
    FreeAndNil(frmProgress);
  end;
end;

procedure TfrmGameManager.actGroupByDeveloperExecute(Sender: TObject);
begin
  GroupMode := lvGMDeveloper;
  UpdateVTVGameList;
end;

procedure TfrmGameManager.actGroupByFamilyExecute(Sender: TObject);
begin
  GroupMode := lvGMGameGroup;
  UpdateVTVGameList;
end;

procedure TfrmGameManager.actGroupByFolderExecute(Sender: TObject);
begin
  GroupMode := lvGMFolder;
  UpdateVTVGameList;
end;

procedure TfrmGameManager.actGroupByNameExecute(Sender: TObject);
begin
  GroupMode := lvGMName;
  UpdateVTVGameList;
end;

procedure TfrmGameManager.actGroupByPublisherExecute(Sender: TObject);
begin
  GroupMode := lvGMPublisher;
  UpdateVTVGameList;
end;

procedure TfrmGameManager.actGroupByTagsExecute(Sender: TObject);
begin
  GroupMode := lvGMTags;
  UpdateVTVGameList;
end;

procedure TfrmGameManager.actGroupByYearExecute(Sender: TObject);
begin
  GroupMode := lvGMYear;
  UpdateVTVGameList;
end;

procedure TfrmGameManager.actImportSystemDataExecute(Sender: TObject);
begin
  OpenDialog.Filter := rsEmutecaGameDatabase + CDBFilter;
  OpenDialog.DefaultExt := CDBExt;
  if not OpenDialog.Execute then
    Exit;

  vstGames.Clear;
  Application.CreateForm(TfrmProgress, frmProgress);
  try
    GameManager.ImportGameData(OpenDialog.FileName);
  finally
    FreeAndNil(frmProgress);
  end;

  UpdateVTVGameList;
end;

procedure TfrmGameManager.actLockGameTextExecute(Sender: TObject);
begin
  memoGame.ReadOnly := actLockGameText.Checked;
end;

procedure TfrmGameManager.actLockSystemTextExecute(Sender: TObject);
begin
  memoSystem.ReadOnly := actLockSystemText.Checked;
end;

procedure TfrmGameManager.actMediaManagerExecute(Sender: TObject);
var
  FormMM: TfrmMediaManager;
begin
  Application.CreateForm(TfrmMediaManager, FormMM);
  try
    FormMM.Config := Self.Config;
    FormMM.GameManager := Self.GameManager;
    FormMM.ShowModal;
  finally
    FreeAndNil(FormMM);
  end;
end;

procedure TfrmGameManager.actNextGameImageExecute(Sender: TObject);
begin
  NextGameImage;
end;

procedure TfrmGameManager.actOpenSystemFolderExecute(Sender: TObject);
begin
  if GameManager.System = nil then
    Exit;
  if not DirectoryExistsUTF8(GameManager.System.BaseFolder) then
    Exit;
  OpenDocument(GameManager.System.BaseFolder);
end;

procedure TfrmGameManager.actPasteGameIconImageExecute(Sender: TObject);
begin
  PasteGameIconImage;
end;

procedure TfrmGameManager.actPasteGameImageExecute(Sender: TObject);
begin
  PasteGameImage;
end;

procedure TfrmGameManager.actPasteGameSpineImageExecute(Sender: TObject);
begin
  PasteGameSpineImage;
end;

procedure TfrmGameManager.actPlayGameExecute(Sender: TObject);
var
  ExitCode: integer;
begin
  if CurrGame = nil then
    Exit;

  Self.Enabled := False;
  Application.Minimize;
  try
    ExitCode := GameManager.Execute(CurrGame);

    case ExitCode of
      0: ; // Do nothing. Maybe I must show airbursts
      CGMExecErrorNoGame: // Emuteca's error code
        ShowMessage(Format(rsErrorGameNotFound,
          [CurrGame.Folder, CurrGame.FileName]));
      else
        // User must search emulator doc the meaning of code as homework
        ShowMessage(Format(rsErrorEmulator, [ExitCode]));
    end;
  finally
    Self.Enabled := True;
    Application.Restore;
    if Self.CanFocus then
      Self.SetFocus;
  end;
end;

procedure TfrmGameManager.actPreviousGameImageExecute(Sender: TObject);
begin
  PreviousGameImage;
end;

procedure TfrmGameManager.actPurgeSystemDataExecute(Sender: TObject);
begin
  if MessageDlg(rsPurgeMessage, mtWarning, [mbYes, mbNo], 0) = mrNo then
    Exit;
  GameManager.PurgeGameData;
  UpdateVTVGameList;
end;

procedure TfrmGameManager.actScriptManagerExecute(Sender: TObject);
var
  FormSM: TfrmScriptManager;
begin
  if GameManager.System = nil then
    Exit;

  // Fix a posible crash while updating te form, after closing the
  //   Script Manager, because games or groups can have been modified.
  vstGames.Clear;

  Application.CreateForm(TfrmScriptManager, FormSM);
  try
    FormSM.Config := Self.Config;
    FormSM.GameManager := GameManager;
    FormSM.ShowModal;
  finally
    FreeAndNil(FormSM);
  end;

  UpdateVTVGameList;
end;

procedure TfrmGameManager.actShowGroupTypeMenuExecute(Sender: TObject);
begin
  pmGroupType.PopUp;
end;

procedure TfrmGameManager.actEmulatorManagerExecute(Sender: TObject);
begin
  Application.CreateForm(TfrmEmulatorManager, frmEmulatorManager);
  try
    frmEmulatorManager.Config := Self.Config;
    frmEmulatorManager.ShowModal;
  finally
    FreeAndNil(frmEmulatorManager);
  end;
end;

procedure TfrmGameManager.actEmulatorFolderExecute(Sender: TObject);
begin
  OpenEmulatorFolder;
end;

procedure TfrmGameManager.actAboutExecute(Sender: TObject);
var
  FormAbout: TfrmAbout;
begin
  Application.CreateForm(TfrmAbout, FormAbout);
  try
    FormAbout.Config := Self.Config;
    FormAbout.GameManager := GameManager;
    FormAbout.ShowModal;
  finally
    FreeAndNil(FormAbout);
  end;
end;

procedure TfrmGameManager.actChangeGameListFontExecute(Sender: TObject);
var
  TmpFontQuality: TFontQuality;
begin
  TmpFontQuality := vstGames.Font.Quality;
  FontDialog.Font.Assign(vstGames.Font);
  if not FontDialog.Execute then Exit;
  vstGames.Font.Assign(FontDialog.Font);
  vstGames.Font.Quality := TmpFontQuality;
  vstGames.DefaultNodeHeight := abs(vstGames.Font.Height) * 2;

  // TODO 2: Change height of existing nodes

  Self.Invalidate;
end;

procedure TfrmGameManager.actConfigManagerExecute(Sender: TObject);
var
  FormCM: TfrmConfigManager;
begin
  Application.CreateForm(TfrmConfigManager, FormCM);
  try
    FormCM.Config := Self.Config;
    FormCM.ShowModal;
  finally
    FreeAndNil(FormCM);
  end;
end;

procedure TfrmGameManager.actEmulatorWebPageExecute(Sender: TObject);
begin
  if GameManager.Emulator = nil then
    Exit;
  OpenURL(GameManager.Emulator.WebPage);
end;

procedure TfrmGameManager.actQuickUpdateListExecute(Sender: TObject);
begin
  // TODO 4: Hacer actualización rápida.
  Self.actUpdateGameListExecute(Sender);
end;

procedure TfrmGameManager.actRunEmulatorExecute(Sender: TObject);
begin
  Self.Enabled := False;
  Application.Minimize;
  try
    ExitCode := GameManager.Emulator.ExecuteAlone;

    case ExitCode of
      0: ; // Do nothing. Maybe I must show airbursts
      CGMExecErrorNoGame: // Emuteca's error code
        ShowMessage(Format(rsErrorGameNotFound,
          [CurrGame.Folder, CurrGame.FileName]));
      else
        // User must search emulator doc the meaning of code as homework
        ShowMessage(Format(rsErrorEmulator, [ExitCode]));
    end;
  finally
    Self.Enabled := True;
    Application.Restore;
    if Self.CanFocus then
      Self.SetFocus;
  end;
end;

procedure TfrmGameManager.actSaveEditorDataExecute(Sender: TObject);
begin
  if CurrGame <> nil then
    SaveGameData
  else if CurrGroup <> nil then
    SaveGroupData;
end;

procedure TfrmGameManager.actSaveEmuTextExecute(Sender: TObject);
begin
  if GameManager.Emulator = nil then
    Exit;
  if not FileExistsUTF8(GameManager.Emulator.InfoFile) then
  begin
    // TODO 1: Make localizable
    SaveDialog.Filter := 'Text file (*.txt)|*.txt';
    SaveDialog.DefaultExt := '*.txt';
    SaveDialog.FileName := GameManager.Emulator.InfoFile;
    if not SaveDialog.Execute then
      Exit;
    GameManager.Emulator.InfoFile := SaveDialog.FileName;
    GameManager.Emulator.SaveToFile(Config.DataFolder +
      Config.EmulatorsIniFile, False);
  end;

  SaveTextToFile(memoEmulator.Lines, GameManager.Emulator.InfoFile);
end;

procedure TfrmGameManager.actSaveGameListExecute(Sender: TObject);
begin
  Application.CreateForm(TfrmProgress, frmProgress);
  try
    GameManager.SaveSystemGameList;
  finally
    FreeAndNil(frmProgress);
  end;
end;

procedure TfrmGameManager.actSaveGameTextExecute(Sender: TObject);
begin
  SaveGameText;
end;

procedure TfrmGameManager.actSavePropertiesExecute(Sender: TObject);
begin
  if CurrGame = nil then
    Exit;

  CurrGame.Alternate := eAlternate.Text;
  CurrGame.Verified := chkVerified.Checked;
  CurrGame.BadDump := eBadDump.Text;

  CurrGame.Pirate := ePirate.Text;
  CurrGame.License := eLicense.Text;

  CurrGame.Languages.CommaText := eLanguages.Text;
  CurrGame.Translation := eTranslation.Text;

  CurrGame.Cracked := eCracked.Text;
  CurrGame.Fixed := eFixed.Text;
  CurrGame.Modified := eModified.Text;
  CurrGame.Trainer := eTrainer.Text;
  CurrGame.Hack := eHack.Text;

  vstGames.Refresh;
end;

procedure TfrmGameManager.actSaveSystemTextExecute(Sender: TObject);
begin
  if GameManager.System = nil then
    Exit;
  if not FileExistsUTF8(GameManager.System.InfoText) then
  begin
    // TODO 1: Make localizable
    SaveDialog.Filter := 'Text file (*.txt)|*.txt';
    SaveDialog.DefaultExt := '*.txt';
    SaveDialog.FileName := GameManager.System.InfoText;
    if not SaveDialog.Execute then
      Exit;
    GameManager.System.InfoText := SaveDialog.FileName;
    GameManager.System.SaveToFile(Config.DataFolder + Config.SystemsIniFile, False);
  end;

  SaveTextToFile(memoSystem.Lines, GameManager.System.InfoText);
end;

procedure TfrmGameManager.actShowMainMenuExecute(Sender: TObject);
begin
  pmMainMenu.PopUp();
end;

procedure TfrmGameManager.actSystemManagerExecute(Sender: TObject);
var
  FormSM: TfrmSystemManager;
  FormResult: TModalResult;
begin
  Application.CreateForm(TfrmSystemManager, FormSM);
  try
    FormSM.Config := Self.Config;
    FormResult := FormSM.ShowModal;

    if FormResult = mrOk then
    begin
      // ¿Fix a posible crash while updating te form?
      vstGames.Clear;

      GameManager.SaveSystemGameList;
      UpdateSystemList;
    end;

  finally
    FreeAndNil(FormSM);
  end;
end;

procedure TfrmGameManager.actUpdateGameListExecute(Sender: TObject);
begin
  if GameManager.System = nil then
    Exit;

  vstGames.Clear;
  Self.Enabled := False;
  Application.CreateForm(TfrmProgress, frmProgress);
  try
    GameManager.UpdateGameList;
  finally
    FreeAndNil(frmProgress);
  end;
  Self.Enabled := True;
  if Self.CanFocus then
    Self.SetFocus;

  UpdateVTVGameList;
end;

procedure TfrmGameManager.actViewEmulatorImageExecute(Sender: TObject);
begin
  OpenEmulatorImages;
end;

procedure TfrmGameManager.actViewGameImageExecute(Sender: TObject);
begin
  OpenGameImages;
end;

procedure TfrmGameManager.actViewSystemImageExecute(Sender: TObject);
begin
  OpenSystemImages;
end;

procedure TfrmGameManager.cbEmulatorsSelect(Sender: TObject);
begin
  ChangeCurrentEmulator;
end;

procedure TfrmGameManager.cbGameGroupSelect(Sender: TObject);
begin
  // The group of the game was changes so we must update group tags
  lGroupTags.Enabled := True;
  mmGroupTags.Enabled := True;
  mmGroupTags.Clear;
  mmGroupTags.Lines.AddStrings(
    cGameGroup(cbGameGroup.Items.Objects[cbGameGroup.ItemIndex]).Tags);
  mmGroupTags.Modified := False;
end;

procedure TfrmGameManager.cbGameImagesChange(Sender: TObject);
begin
  if CurrGame <> nil then
    UpdateGameImage
  else
  if CurrGroup <> nil then
    UpdateGroupImage;
end;

procedure TfrmGameManager.cbGameTextsChange(Sender: TObject);
begin
  if CurrGame <> nil then
    UpdateGameText
  else
  if CurrGroup <> nil then
    UpdateGroupText;
end;

procedure TfrmGameManager.cbSearchChange(Sender: TObject);
begin
  SearchGames;
end;

procedure TfrmGameManager.cbSystemDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  RectIcon: TRect;
  aCBX: TComboBox;

begin
  if odInactive in State then Exit;
  aCBX := TComboBox(Control);

  // Icon
  aCBX.Canvas.FillRect(ARect);
  RectIcon := ARect;
  RectIcon.Left := RectIcon.Left + 1;
  RectIcon.Right := RectIcon.Left + ARect.Bottom - ARect.Top;
  if Index < SystemIcons.Count then
    aCBX.Canvas.StretchDraw(RectIcon, SystemIcons[Index].Graphic);

  // Text
  aCBX.Canvas.TextOut(RectIcon.Right + 2, ARect.Top, aCBX.Items[Index]);
end;

procedure TfrmGameManager.cbSystemSelect(Sender: TObject);
begin
  ChangeCurrentSystem;
end;

procedure TfrmGameManager.ePropertiesKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
    begin
      if Shift = [] then
      begin
        actSaveProperties.Execute;
        Key := 0;
      end;
    end;
  end;
end;

procedure TfrmGameManager.eEditorKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
    begin
      if Shift = [] then
      begin
        actSaveEditorData.Execute;
        Key := 0;
      end;
    end;
  end;
end;

procedure TfrmGameManager.eNameChange(Sender: TObject);
begin
  if CurrGame = nil then
    Exit;
end;

procedure TfrmGameManager.eSearchChange(Sender: TObject);
begin
  SearchGames;
end;

initialization
  {$I fGameManager.lrs}

end.

