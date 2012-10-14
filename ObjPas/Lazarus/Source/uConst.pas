unit uConst;

{$mode objfpc}{$H+}

interface

const

  CDBExt = '.edb';
  CDBFilter = ' (*' + CDBExt + ')|*' + CDBExt;

  kFSMSystemIniFilter = 'Systems Ini File (*.ini)';

  kFSMScriptExt = '.pas';
  kFSMScriptFilter = ' (*' + kFSMScriptExt + ')|*' +
    kFSMScriptExt + '|All files|' + AllFilesMask;
  kFSMUnitsFolder = 'Common/';
  kFSMDataSection = 'SCRIPTDATA';

  kFABCopyright = '(C) 2006-2012 Chixpy';
  kFABLicense = 'GNU GPL v3';

  kFEMEmulatorsFileExt = '.ini';

  kSimilarityThresold = 25;

implementation

end.

