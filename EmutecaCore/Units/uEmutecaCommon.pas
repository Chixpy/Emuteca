unit uEmutecaCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sha1;

const
  { TODO : Change prefix to krs }
  { TODO : Not all must be global constants... }

  krsEmuteca = 'Emuteca';
  {< Main 'Emuteca' String!!! }

  krsEmutecaGameSubFolder = 'Game/';
  {< Subfolder in temp directory, where games will be decompressed.

    Please attach directory separator}

  // Extensions
  // ----------
  krsEmutecaGroupFileExt = '.egl';
  {< Extension for group lists. }
  krsEmutecaSoftFileExt = '.csv';
  {< Extension for soft lists. }

  // EXIT CODES for handling some errors
  // Praying for no emulator use these exit codes.
  kEmutecaExecErrorNoGame = -300;
  {< Error code when game is not found. }
  kEmutecaDecompressError = -301;
  {< Error decompressing archive. }

  kEmuTKSHA1Empty:TSHA1Digest = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

  // IniKeys
  // -------
  // Shared
  krsIniKeyID = 'ID';
  krsIniKeyTitle = 'Title';
   krsIniKeyFileName = 'FileName';
  krsIniKeyYear = 'Year';

   // System
   krsIniKeyEnabled = 'Enabled';
   krsIniKeyExtensions = 'Extensions';
   krsIniKeyBaseFolder = 'BaseFolder';
   krsIniKeyTempFolder = 'TempFolder';
   krsIniKeyMainEmulator = 'MainEmulator';
   krsIniKeyOtherEmulators = 'OtherEmulators';
   krsIniKeyIcon = 'Icon';
   krsIniKeyImage = 'Image';
   krsIniKeyBackImage = 'BackImage';
   krsIniKeyIconFolder = 'IconFolder';
   krsIniKeyImageFolders = 'ImageFolders';
   krsIniKeyImageCaptions = 'ImageCaptions';
   krsIniKeyText = 'Text';
   krsIniKeyTextFolders = 'TextFolders';
   krsIniKeyTextCaptions = 'TextCaptions';
   krsIniKeyGamesKey = 'GamesKey';
   krsIniKeyExtractAll = 'ExtractAll';

  // Group
  krsIniKeyDeveloper = 'Developer';

  // Soft
  krsIniKeyGroup = 'Group';
  krsIniKeyTranslitTitl = 'TranslitTitle';
  krsIniKeySortTitle = 'SortTitle';
  krsIniKeyVersion = 'Version';
  krsIniKeyPublisher = 'Publisher';
  krsIniKeyZone = 'Zone';
  krsIniKeyDumpInfo = 'DumpInfo';
  krsIniKeyDumpStatus = 'DumpStatus';
  krsIniKeyFixed = 'Fixed';
  krsIniKeyTrainer = 'Trainer';
  krsIniKeyTranslation = 'Translation';
  krsIniKeyPirate = 'Pirate';
  krsIniKeyCracked = 'Cracked';
  krsIniKeyModified = 'Modified';
  krsIniKeyHack = 'Hack';
  krsIniKeyFolder = 'Folder';



resourcestring

  // Game / Group property names
  // ---------------------------
  rsGameKey = 'Key';
  {< Game Key property. }
  rsZones = 'Zones';
  {< Game Zones property. }
  rsDeveloper = 'Developer';
  {< Game Developer property. }
  rsPublisher = 'Publisher';
  {< Game Publisher property. }
  rsVersion = 'Version';
  {< Game Version property. }
  rsFilename = 'Filename';
  {< Game Filename property. }

  // Formated statistics
  // -------------------
  rsFmtNVersions = '%0:d versions.';
  rsFmtNItems = '%1:d visible of %0:d items.';
  {< %0:d = Number of items. }

  rsFmtNTimes = '%0:d times.';
  {< %0:d = Number of times. }

type
  TEmutecaProgressCallBack = function(const Title, Info1, Info2: string;
    const Value, MaxValue: int64): boolean of object;
  {< Callback funtion to show progress }

implementation

end.
