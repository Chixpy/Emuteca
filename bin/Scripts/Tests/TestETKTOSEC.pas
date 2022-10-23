{
[Info]
This script tests uTOSECUtils.
[Data]
Name=Chixpy
Version=
Date=20221023
[Changes]

[EndInfo]
}
program TestCHXStrUtils;

//uses uTOSECUtils;

{$I '../Units/uTOSECUtils.pas'}
var
  i: integer;
  aStr: string;
  TOSECTests: array of string;
  TOSECOutput: array of string;

begin
  WriteLn('Hello!');
  WriteLn('');
  WriteLn('This Script will test uTOSECUtils.');
  WriteLn('');

  TOSECTests := [
    'Soft Title, The v 1 (demo) (Year)(Publisher)(A3000)(MCGA)(US)' +
    '(en-es)(SW)(Devstatus)(Media Type)(Media Label)[cr][f][h][m]' +
    '[p][t][tr][o][u][v][b][a][!][more info]',
    'Soft Title (Year)(Publisher)',
    'Soft Title v10, The (Year)(Publisher)',
    'Soft Title, The v10 (Year)(Publisher)',
    'Soft Title v 10, The (Year)(Publisher)',
    'Soft Title, The v 10 (Year)(Publisher)',
    'Soft Title, The v 10 (Year)',
    'Soft Title, The v 10 (Publisher)',
    ''
    ];

  // Expected output
  TOSECOutput := [
    // 0 - Full Flags
    'This error is only to show full flags',
    // 1 - Soft Title (Year)(Publisher)
    '"@",,"1","@","@","Soft Title","@","","","Year","Publisher",' +
    '"","","","","","","","","",""',
    // 2 - Soft Title v10, The (Year)(Publisher)
    '"@",,"2","@","@","The Soft Title","@","Soft Title","v10","Year",' +
    '"Publisher","","","","","","","","","",""',
    // 3 - Soft Title, The v10 (Year)(Publisher)
    '"@",,"3","@","@","The Soft Title","@","Soft Title","v10","Year",' +
    '"Publisher","","","","","","","","","",""',
    // 4 - Soft Title v 10, The (Year)(Publisher)
    '"@",,"4","@","@","The Soft Title","@","Soft Title","v10","Year",' +
    '"Publisher","","","","","","","","","",""',
    // 5 - Soft Title, The v 10 (Year)(Publisher)
    '"@",,"5","@","@","The Soft Title","@","Soft Title","v10","Year",' +
    '"Publisher","","","","","","","","","",""',
    // 6 - Soft Title, The v 10 (Year) <- NO PUBLISHER
    '',
    // 7 - Soft Title, The v 10 (Publisher) <- NO YEAR (Gives publisher error)
    '',
    // n - Empty
    ''
    ];

  if Length(TOSECTests) > Length(TOSECOutput) then
    SetLength(TOSECOutput, Length(TOSECTests));

  WriteLn(krsCSVSoftHeader);
  WriteLn('');

  i := Low(TOSECTests);
  while i <= High(TOSECTests) do
  begin
    WriteLn(TOSECTests[i]);
    aStr := TOSECExtractInfo(IntToStr(i) + TOSECIDSep + TOSECTests[i]);
    if aStr = TOSECOutput[i] then
    begin
      WriteLn('^---- OK');
    end
    else
    begin
      WriteLn('OUTPUT : ' + aStr);
      WriteLn('CORRECT: ' + TOSECOutput[i]);
      WriteLn('^---- WRONG');
    end;
    WriteLn('');
    Inc(i);
  end;  


  WriteLn('');
  WriteLn('DONE');
  WriteLn('----');

end.
