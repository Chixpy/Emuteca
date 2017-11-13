{
[Info]
Some common functions for string handling
[Data]
Name=Chixpy
Version=0.01
Date=20171112
[Changes]

[EndInfo]
}

var
  ETKArticles: Array of string;

procedure ETKStrUtilInit;
begin
  ETKArticles := ['The', 'A', 
    'El', 'La', 'Los', 'Las', 'Un', 'Una', 'Unos', 'Unas'];
end;
  
procedure ETKFixTitle(aOrigTitle: string; var NewTitle, SortTitle, 
  MediaFile: string);
var
  aPos: integer;
  Found: 
begin
  if Length(ETKArticles) = 0 then ETKStrUtilInit;

  // Replacing ' - ' with ': '
  NewTitle := AnsiReplaceText(aOrigTitle, ' - ', ': ');
  
  // Searching if NewTitle has an article.
  //   - The Title -> (Must be in NewTitle)
  //   - Title, The -> (Must be SortTitle)
  //   - Title (The) -> Covert to previous
  
  
  if AnsiCompareText(NewTitle, SortTitle) = 0 then 
    SortTitle = '';
  
  // Setting MediaFile
  if SortTitle <> '' then
    MediaFile := CleanFileName(SortTitle, true, false)
  else    
    MediaFile := CleanFileName(NewTitle, true, false)
  



end;
