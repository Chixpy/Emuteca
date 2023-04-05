{
[Info]
This script test Emuteca.
[Data]
Name=Chixpy
Version=0.01
Date=20230201
[Changes]
1.00 - 20230201
  + Added full Emuteca structure examples and tests.
0.01 - 20170919
  + Initial version
[EndInfo]
}
program TestuEmuteca;

var
  SysMan: cEmutecaSystemManager;
  EmuMan: cEmutecaEmulatorManager;
  Sys: cEmutecaSystem;
  i: integer;
  
begin
  WriteLn('This script tests some basic properties of cEmuteca and');
  WriteLn('  other classes in Emuteca''s structure.');
  WriteLn('');
  WriteLn('EMUTECA OBJECT');
  WriteLn('==============');
  WriteLn('');  
  WriteLn('v Emuteca: cEmuteca := Assigned to current Emuteca object');
  WriteLn('  "Emuteca" varible is defined by default with current Emuteca');
  WriteLn('    object.');
  WriteLn('');
  WriteLn('p Emuteca.LoadConfig(aFile: string);');
  WriteLn('  Loads a new config file, like if diferent user. Really multiple');
  WriteLn('    users management is not implemented, but its posible to have');
  WriteLn('    different configurations.');  
  WriteLn('');  
  WriteLn('rw Emuteca.BaseFolder: string;');
  WriteLn('  Base folder of Emuteca, for relative file search.');
  WriteLn('    ' + Emuteca.BaseFolder);
  WriteLn('');
  WriteLn('rw Emuteca.TempFolder: string;');
  WriteLn('  Temporal folder used by Emuteca.');
  WriteLn('    ' + Emuteca.TempFolder);
  WriteLn('');

  WriteLn('r Emuteca.CurrentGroupList: cEmutecaGroupList;');
  WriteLn('  Current group list. Listed in main window, with filters and '); 
  WriteLn('    search applied.');  
  
  WriteLn('');    
  WriteLn('p Emuteca.ClearAllData;');
  WriteLn('p Emuteca.LoadAllData;');  
  WriteLn('p Emuteca.SaveAllData;');
  WriteLn('  Saves current state of Emuteca, games, groups, emulators and systems.');

  WriteLn('');
  WriteLn('procedure CleanSystems;');
  WriteLn('procedure CacheData;');
    
  WriteLn('');
  WriteLn('function Emuteca.RunSoftware(const aSoftware: cEmutecaSoftware): integer');
  WriteLn('  Runs a Soft with its System current Emulator.');  
  WriteLn('');
  WriteLn('Emuteca.Config: cEmutecaConfig;');
  WriteLn('Emuteca.SystemManager: cEmutecaSystemManager;');
  WriteLn('Emuteca.EmulatorManager: cEmutecaEmulatorManager;');
  WriteLn('  Properties to access Config, System Manager and Emulator Manager.');
  WriteLn('');
  WriteLn('Emuteca.ProgressCallBack(const aAction, aInfo: string; const aValue, aMaxValue: int64; const IsCancelable: boolean): boolean;');
  WriteLn('  Property which you can call as a function to show a progress window.'); 
  WriteLn('    - aAction: String showing what action is performed.'); 
  WriteLn('    - aInfo: Some extra information of the action, for example current item.'); 
  WriteLn('    - aValue: Current item number.'); 
  WriteLn('      If it''s equal or greater that aMaxValue then the window is autoclosed.'); 
  WriteLn('    - aMaxValue: Total number of items.'); 
  WriteLn('    - IsCancelable: Can the user cancel the process?'); 
  WriteLn('    - Result: If true continue processing.'); 

  WriteLn('');
  WriteLn('procedure UpdateSysEmulators;');
  WriteLn('procedure UpdateCurrentGroupList(aSystem: cEmutecaSystem; const aWordFilter: string; aFileList: TStrings);');


  WriteLn('');
  WriteLn('EMUTECA''s EMULATOR MANAGER');
  WriteLn('==========================');  
  WriteLn('');
  
  WriteLn('Emuteca.EmulatorManager: cEmutecaEmulatorManager;');
  WriteLn('  It''s the system manager of Emuteca.');
  WriteLn('  Basically it stores the logic tree of Emuteca.');
  EmuMan := Emuteca.EmulatorManager;
  
  WriteLn('');
  WriteLn('EMUTECA''s SYSTEM MANAGER');
  WriteLn('========================');
  
  WriteLn('');
  WriteLn('Emuteca.SystemManager: cEmutecaSystemManager;');
  WriteLn('  It''s the system manager of Emuteca.');
  WriteLn('  Basically it stores the logic tree of Emuteca.');
  SysMan := Emuteca.SystemManager;

  WriteLn('');
  WriteLn('Emuteca.SystemManager.TempFolder: string;');
  WriteLn('  Temp folder used by system manager.');
  WriteLn('  Usually the same Emuteca''s temp folder.'); 
  WriteLn('    TempFolder: ' + SysMan.TempFolder);   
  
  WriteLn('');
  WriteLn('Emuteca.SystemManager.SysDataFolder: string;');
  WriteLn('  Folder where systems data is stored.'); 
  WriteLn('    SysDataFolder: ' + SysMan.SysDataFolder); 
  
  WriteLn('');
  WriteLn('Emuteca.SystemManager.FullList: cEmutecaSysList;');
  WriteLn('Emuteca.SystemManager.EnabledList: cEmutecaSysList;');
  WriteLn('  Lists of systems with all systems and only enabled ones.');
  WriteLn('  FullList owns the system objects.');

  WriteLn('');
  WriteLn('procedure Emuteca.SystemManager.UpdateEnabledList;');
  WriteLn('  Updates system enabled list.'); 
  
  // Don't make an example.
  // SysMan.ClearData; 
  // SysMan.ProgressCallBack;
    WriteLn('');
    WriteLn('function AddSystem(aID: string): cEmutecaSystem;');
    
    WriteLn('procedure LoadSystemData(aSystem: cEmutecaSystem);');
    WriteLn('procedure SaveSystemData(aSystem: cEmutecaSystem; ClearFile: Boolean);');
    WriteLn('procedure LoadAllEnabledSystemsData;');
    WriteLn('procedure SaveAllEnabledSystemsData;');
  
  WriteLn('');
  WriteLn('SYSTEM LISTS (cEmutecaSysList)');
  WriteLn('==============================');
  WriteLn('');
  WriteLn('Both lists (Enabled and full) have the same properties.');
  
  WriteLn('');
  WriteLn('cEmutecaSysList.Count: integer;');
  WriteLn('  Number of systems in the list.');
  WriteLn('    FullList systems: ' + IntToStr(SysMan.FullList.Count));  
  WriteLn('    EnabledList systems: ' + IntToStr(SysMan.EnabledList.Count));

  WriteLn('');
  WriteLn('cEmutecaSysList.Items[x]: cEmutecaSystem; (you can use  cEmutecaSysList[x])');
  WriteLn('  Returns the system in the position "x" of the list.');

  if SysMan.FullList.Count > 0 then
  begin
    i := RandomInt(0, SysMan.FullList.Count - 1);
    Sys := SysMan.FullList[i];

    WriteLn('');
    WriteLn('');
    WriteLn('Random system properties: Emuteca.SystemManager.FullList[' +
      IntToStr(i) + ']');

    WriteLn('  TempFolder: ' + Sys.TempFolder);

    WriteLn('  ID: ' + Sys.ID);
    WriteLn('  Title: ' + Sys.Title);
    WriteLn('  ListFileName: ' + Sys.ListFileName);
    WriteLn('  Enabled: ' + BoolToStr(Sys.Enabled));
    WriteLn('  ExtractAll: ' + BoolToStr(Sys.ExtractAll));
    WriteLn('  MergeableGroups: ' + BoolToStr(Sys.MergeableGroups));
    WriteLn('  BaseFolder: ' + Sys.BaseFolder);
    WriteLn('  WorkingFolder: ' + Sys.WorkingFolder);

    WriteLn('  MainEmulator: ' + Sys.MainEmulator);
    WriteLn('  OtherEmulators: ' + Sys.OtherEmulators.CommaText);
    WriteLn('  CoreIDs: ' + Sys.CoreIDs.CommaText);

    WriteLn('  IconFile: ' + Sys.IconFile);
    WriteLn('  ImageFile: ' + Sys.ImageFile);
    WriteLn('  BackgroundFile: ' + Sys.BackgroundFile);
    WriteLn('  SoftIconFile: ' + Sys.SoftIconFile);

    WriteLn('  IconFolder: ' + Sys.IconFolder);
    WriteLn('  LogoFolder: ' + Sys.LogoFolder);
    WriteLn('  ImageFolders: ' + Sys.ImageFolders.CommaText);
    WriteLn('  ImageCaptions: ' + Sys.ImageCaptions.CommaText);

    WriteLn('  InfoText: ' + Sys.InfoText);
    WriteLn('  TextFolders: ' + Sys.TextFolders.CommaText);
    WriteLn('  TextCaptions: ' + Sys.TextCaptions.CommaText);

    WriteLn('  MusicFolders: ' + Sys.MusicFolders.CommaText);
    WriteLn('  MusicCaptions: ' + Sys.MusicCaptions.CommaText);

    WriteLn('  VideoFolders: ' + Sys.VideoFolders.CommaText);
    WriteLn('  VideoCaptions: ' + Sys.VideoCaptions.CommaText);

    WriteLn('  SoftExportKey: ' + SoftExportKey2StrK(Sys.SoftExportKey));
    WriteLn('  Extensions: ' + Sys.Extensions.CommaText);

    WriteLn('  Stats.LastTime (LastTimeStr): ' + Sys.Stats.LastTimeStr);
    WriteLn('  Stats.TimesPlayed (TimesPlayedStr): ' + Sys.Stats.TimesPlayedStr);
    WriteLn('  Stats.PlayingTime (PlayingTimeStr): ' + Sys.Stats.PlayingTimeStr);
  end;
  
  WriteLn('');
  WriteLn('DONE.');
end.
