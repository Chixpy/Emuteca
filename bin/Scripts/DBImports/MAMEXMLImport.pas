{
[Info]
This script creates a database from MAME XML. XML file can be created with

MAME -listxml > mame.xml

or downloaded from MAMEDevs page.

It's better than -listfull + -listclones, because
year, zone and manufacturer can be extracted

[Data]
Name=Chixpy
Version=0.01
Date=20181
[Changes]
0.01
  + Initial working version
[EndInfo]
}
program MAMEXMLImport;
// uses
//  uETKFileUtils, uETKMAMEUtils;
{$I '../Units/uETKFileUtils.pas'}
{$I '../Units/uETKMAMEUtils.pas'}

var
  XMLFilename, OutFilename, VersionStr, ParentStr: string;
  XMLFile, VersionList, outVList, outPList, aSL: TStringList;  
  i: integer;

begin
  OutFilename := AskFile('Database file for output',
    'Emuteca soft DB|' + krsFileMaskSoft, 'MAME' + krsFileExtSoft);
  if OutFilename = '' then
  begin
    WriteLn('Output file not assigned.');
    Exit;
  end;
  
  if ExtractFileExt(OutFilename) = '' then
    OutFilename := OutFilename + '.csv';

  XMLFilename := AskFile(
    'XML with mame data',
    'XML files (*.xml)|*.xml', '');
  if not TestFilename(XMLFilename) then Exit;


  WriteLn('Loading file, this can take a while...');
  WriteLn('');

  VersionList := CreateStringList;
  XMLFile := CreateStringList;
  outVList := CreateStringList;
  outPList := CreateStringList;
  aSL := CreateStringList;
  try
    WriteLn('Reading: ' + XMLFilename);
    
    XMLFile.BeginUpdate;
    XMLFile.LoadFromFile(XMLFilename);
    XMLFile.EndUpdate;
    
    WriteLn(IntToStr(XMLFile.Count) + ' lines readed.');
    
    WriteLn('');
    WriteLn('Searching parents and versions...');
    WriteLn('');
    
    MAMEReadXMLFile(XMLFile, VersionList);
    
    // VersionList CommaText
    // 0.- ID
    // 1.- Parent ID
    // 2.- Title
    // 3.- Year
    // 4.- Publisher

    WriteLn('');
    WriteLn('Extracting version info from titles...');
    WriteLn(IntToStr(VersionList.Count) + ' versions found.');
    WriteLn('');
    
    outVList.Add(krsCSVSoftHeader) //Adding header
    outPList.Add(krsCSVGroupHeader) //Adding header 

    i := 0;
    while i < VersionList.Count do
    begin
      aSL.CommaText := VersionList[i];
      
      MAMEExtractInfo(aSL, VersionStr, ParentStr);      
      if VersionStr <> '' then
        outVList.Add(VersionStr);  
      if ParentStr <> '' then
        outPList.Add(ParentStr);         
    
      Inc(i);
      
      // After Inc(i) is the actual number of files analized.
      if (i and 2047) = 2047 then
        WriteLn(IntToStr(i) + ' soft files added.');
    end;

    WriteLn('');
    WriteLn('Saving: ' + OutFilename);
    outVList.SaveToFile(OutFilename);
    WriteLn('Saving: ' + ChangeFileExt(OutFilename, '.egl'));
    outPList.SaveToFile(ChangeFileExt(OutFilename, '.egl'));

    WriteLn('');
    WriteLn('Finished!');
  finally
    XMLFile.Free;
    VersionList.Free;
    outVList.Free;
    outPList.Free;
    aSL.Free;
  end;
end.
