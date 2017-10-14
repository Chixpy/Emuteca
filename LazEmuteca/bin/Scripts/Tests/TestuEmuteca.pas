{
[Info]
This script test Emuteca.
[Data]
Name=Chixpy
Version=0.01
Date=20170919
[Changes]

[EndInfo]
}
program TestEmuteca;

begin
  WriteLn('This script test some basic properties of cEmuteca.');
  WriteLn('');
  WriteLn('EMUTECA OBJECT');
  WriteLn('==============');
  WriteLn('');  
  WriteLn('Emuteca: cEmuteca;');
  WriteLn('  "Emuteca" varible is defined by default with current Emuteca');
  WriteLn('    object.');
  WriteLn('');
  WriteLn('Emuteca.BaseFolder: string;');
  WriteLn('  Base folder of Emuteca, for relative file search.');
  WriteLn('    ' + Emuteca.BaseFolder);
  WriteLn('');
  WriteLn('Emuteca.TempFolder: string;');
  WriteLn('  Temporal folder used by Emuteca.');
  WriteLn('    ' + Emuteca.TempFolder);
  WriteLn('');
  WriteLn('procedure Emuteca.SaveData;');
  WriteLn('  Saves current state of Emuteca.');
  WriteLn('');
  WriteLn('function Emuteca.RunSoftware(const aSoftware: cEmutecaSoftware): integer');
  WriteLn('  Runs a Soft. With its System current Emulator.');  
  WriteLn('');
  WriteLn('Emuteca.Config: cEmutecaConfig;');
  WriteLn('Emuteca.SystemManager: cEmutecaSystemManager;');
  WriteLn('Emuteca.EmulatorManager: cEmutecaEmulatorManager;');
  WriteLn('  Properties to access Config, System Manager and Emulator Manager.');
  WriteLn('');
  WriteLn('DONE.');
end.
