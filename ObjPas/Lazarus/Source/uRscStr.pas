unit uRscStr;

{$mode objfpc}{$H+}

interface

resourcestring
  rsFGMAddingFile = 'Adding file:';
  //< Translatable string:
  rsFGMUpdatingList = 'Updating List:';
  //< Translatable string:
  rsFGMLoadingGameList = 'Loading games:';
  //< Translatable string:
  rsFGMSavingGameList = 'Saving games:';
  //< Translatable string:
  rsFGMEmutecaGameDatabase = 'Emuteca game database';
  //< Translatable string:
  rsFGMDecompressing = 'Decompressing:';
  //< Translatable string:
  rsFGMImportingData = 'Importing data:';
  //< Translatable string:
  rsFGMExportingData = 'Exporting data:';
  //< Translatable string:

  rsFGMKey = 'Key';
  //< Translatable string:
  rsFGMZones = 'Zone';
  //< Translatable string:
  rsFGMDeveloper = 'Developer';
  //< Translatable string:
  rsFGMPublisher = 'Publisher';
  //< Translatable string:

  rsFGMVersion = 'Version';
  //< Translatable string:
  rsFGMFilename = 'Filename';
  //< Translatable string:

  rsFGMNGroups = '%0:d groups';
  //< Translatable string:
  rsFGMNGames = '%0:d games';
  //< Translatable string:
  rsFGMNTimes = '%0:d times';
  //< Translatable string:

  rsFGMNever = 'Never';
  //< Translatable string:
  rsFGMUnknown = '!Unknown';
  //< Translatable string:

  rsFGMAssignToGroup = 'Do you want to assign it to the game''s group?';
  //< Translatable string:
  rsFGMChooseImageFileFormat =
    'Do you want to save it in a lossless format:' + slinebreak +
    'YES -> .png (lossless for screenshots)' + slinebreak +
    'NO -> .jpg (better for photographs)';
  //< Translatable string:
  rsFGMDeleteFile = 'Are you sure that you want to delete this file?' + slinebreak +
    '%0:s';
  //< Translatable string:
  rsFGMFolderNotExists = '%0:s' + slinebreak + 'Folder not exists.' + slinebreak +
    'Do you want create it?';
  //< Translatable string:
  rsFGMConfirmOverwriteFile =
    '%0:s' + slinebreak + 'The file already exists.' + slinebreak +
    'Do you want overwrite it?';
  //< Translatable string:

  rsFGMErrorGameNotFound = 'Game not found:' + slinebreak + '%0:s%1:s';
  //< Translatable string:
  rsFGMErrorEmulator = 'Emulator exited with error code: %0:d';
  //< Translatable string:
  rsFGMPurgeMessage = 'Warning:' + slinebreak +
    'This action will erase all the game and group list.' +
    slinebreak + 'Do you want to continue?';
  //< Translatable string:

  rsFGMSystemRequired = 'You need select a system before do this action.';
  //< Translatable string:

  rsFSMScriptFileSaved = 'Script file saved: %0:s';
  rsFSMEmutecaScript = 'Emuteca Script File';
  rsFSMSaveChanges = 'The source was modified:' +
    sLineBreak + '%0:s' + sLineBreak + 'Do you want to save the changes?';
  rsFSMCurrentSystem = 'Current System: %0:s';
  rsFSMCurrentGroup = 'Current Group: %0:s (%1:s)';
  rsFSMCurrentGame = 'Current Game: %0:s (%1:s)';

  rseNotFilename = 'Not defined filename';
  //< Translatable string: 'Not defined filename'

  rsFABCopyright = 'Copyright';
  rsFABUnderLicense = 'Under license %0:s';

    rsSEECompilationMsg = 'Compilation: %0:s';
  rsSEEExecutionMsg = 'Execution: %0:s';
  rsSEEOK = 'OK';
  rsSEEError = 'Error';

    rsFSMSelectSystem = 'Select a system.';
      rsFSMSystemName = 'System name';
      rsFSMAutoConfigSystem = 'This action will create many subfolders in:' +
        sLineBreak + '%0:s' + sLineBreak + 'Are you sure?';
      rsFSMAutoFolderImg = 'Images/';
      rsFSMAutoFolderImgTitle = 'Title/';
      rsFSMAutoFolderImgInGame = 'In game/';
      rsFSMAutoFolderImgFront = 'Front/';
      rsFSMAutoFolderImgMarquee = 'Spine/';
      rsFSMAutoFolderImgBack = 'Back/';
      rsFSMAutoFolderImgMedia = 'Media/';
      rsFSMAutoFolderIcons = 'Icon/';
      rsFSMAutoFolderGames = 'Games/';
      rsFSMAutoFolderMusic = 'Music/';
      rsFSMAutoFolderMusicDemo = 'Demo/';
      rsFSMAutoFolderMusicOST = 'OST/';
      rsFSMAutoFolderMusicMix = 'Mix/';
      rsFSMAutoFolderTemp =  'Temp/';
      rsFSMAutoFolderTxt = 'Texts/';
      rsFSMAutoFolderTxtInfo = 'Information/';
      rsFSMAutoFolderTxtManual = 'Instructions/';
      rsFSMAutoFolderTxtCheat = 'Cheats/';
      rsFSMAutoFolderTxtNotes = 'Notes/';
      rsFSMAutoFolderTxtCredit = 'Credits/';
      rsFSMAutoFolderVideo = 'Videos/';
      rsFSMAutoFolderVideoDemo = 'Demo/';
      rsFSMAutoFolderVideoTAS = 'Inputs/';
      rsFSMAutoFolderVideoInGame = 'In game/';
      rsFSMAutoFolderBIOS = 'BIOS/';
      rsFSMAutoFolderSaves = 'Saves/';
      rsFSMAutoFolderTools =  'Tools/';

      rsEmulatorName = 'Emulator name';
      rsEmulatorIniFilter = 'Emulators Ini File';

      rsfmmSource = 'Source: %0:s';
      //< Translatable string: 'Source: %0:s'
      rsfmmTarget = 'Target: %0:s';
      //< Translatable string: 'Target: %0:s'
      rsfmmDeleteFile = 'Do you want to delete the file?' + sLineBreak +
        sLineBreak + '%0:s';
      //< Translatable string: 'Do you want to delete the file?' + sLineBreak
      //    + sLineBreak + '%0:s'
      rsfmmDeleteAll = 'Do you want to delete all current listed files?' + sLineBreak +
        sLineBreak + 'Folder: %0:s' + sLineBreak + 'Number of files: %1:d';
      //< Translatable string: 'Do you want to delete all current listed files?'
      //    + sLineBreak + sLineBreak + 'Folder: %0:s' + sLineBreak +
      //    'Number of files: %1:d'
      rsfmmDeleteFileError = 'Error deleting the file:' + sLineBreak + '%0:s';
      //< Translatable string: 'Error deleting the file:' + sLineBreak + '%0:s'

      rsfmmTargetExists =
        'Target file already exists.' + sLineBreak + '%0:s' + sLineBreak + 'Do you want to overwrite?';
      //< Translatable string: 'Target file already exists.' + sLineBreak + '%0:s'+
      //    sLineBreak + 'Do you want to overwrite?';

      rsfmmIcons = 'Icons';
      //< Translatable string: 'Icons'
      rsfmmMarquees = 'Spines / Marquees';
      //< Translatable string: 'Spines / Marquees'
      rsfmmDemoMusic = 'Demo music';
      //< Translatable string: 'Demo music'
      rsfmmDemoVideo = 'Demo vídeo';
      //< Translatable string: 'Demo vídeo'

      rsfmmAddingFiles = 'Adding files to the list...';
      //< Translatable string: 'Adding files to the list...'
      rsfmmCopyingFileList = 'Copying file list...';
      //< Translatable string: 'Copying file list...'
      rsfmmSearchFilesWOGroup = 'Searching files without group...';
      //< Translatable string: 'Searching files without group...'
      rsfmmSearchFilesWOGame = 'Searching files without game...';
      //< Translatable string: 'Searching files without game...'

      rsfmmRenameGroup = 'Rename group.';
      rsfmmRenameMediaFileToo ='Do you want to rename group''s media filename ' +
        'to match the new name?';
      rsfmmRenameMediaFile = 'Rename group media filename.';

      rsNFiles = '%0:d files found.';
      //< Translatable string:
implementation

end.

