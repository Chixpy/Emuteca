{
[Info]
Some common functions for string handling
[Data]
Name=Chixpy
Version=0.03
Date=20171205
[Changes]
0.03
  + ETKCopyFrom: Added.
0.02
  + ETKFixTitle: Fixing articles.
0.01
  + ETKStrUtilInit.
  + ETKFixTitle: ' - ' -> ': '
[EndInfo]
}

var
  ETKArticles: Array of string;
  
function ETKCopyFrom(const aStr: string; aPosition: integer): string;
// Copies a string from a position to the end.
begin
  Result := Copy(aStr, aPosition, 1024); // 1024 is an arbitrary constant
end;

procedure ETKFixTitleInit;
// Inits arrays for ETKFixTitle (automatically called)
begin
  ETKArticles := ['The', 'A', 
    'El', 'La', 'Los', 'Las', 'Un', 'Una', 'Unos', 'Unas',
    'L''', 'Le', 'Les', 'Une', 'Des'];
end;
  
procedure ETKFixTitle(var aTitle: string; out SortTitle, MediaFile: string);
var
  i: integer;
  ArticleFound: string;
begin
  if Length(ETKArticles) = 0 then ETKFixTitleInit;

  // Replacing ' - ' with ': '
  aTitle := Trim(AnsiReplaceText(aTitle, ' - ', ': '));
  
  // Searching if aTitle has an article.
  //   - The Title -> (Must be in aTitle)
  //   - Title, The -> (Must be SortTitle)
  //   - Title (The) -> Covert to previous syntax
  
  // To check if already changed and Keeping actual article
  ArticleFound := '';
  
  // (The)
  if ArticleFound = '' then
  begin
    i := Low(ETKArticles);
    while (ArticleFound = '') and (i <= High(ETKArticles)) do
    begin      
      if AnsiEndsStr(' (' + ETKArticles[i] + ')', aTitle) then
        ArticleFound := ETKArticles[i]
      else
        Inc(i);
    end;  
    if ArticleFound <> '' then
    begin
      aTitle := Trim(AnsiLeftStr(aTitle, Length(aTitle) - Length(ArticleFound)
        - 3));
      SortTitle := aTitle + ', ' + ArticleFound;
      aTitle := ArticleFound + ' ' + aTitle;
    end;
  end;
  
  // Title, The
  if ArticleFound = '' then
  begin  
    i := Low(ETKArticles);
    while (ArticleFound = '') and (i <= High(ETKArticles)) do
    begin
      if AnsiEndsStr(', ' + ETKArticles[i], aTitle) then
        ArticleFound := ETKArticles[i]
      else
        Inc(i);
    end;  
    if ArticleFound <> '' then
    begin
      SortTitle := aTitle;
      aTitle := ArticleFound + ' ' + Trim(AnsiLeftStr(aTitle, Length(aTitle)
        - Length(ArticleFound) - 2));
    end; 
  end;  
  
  // The Title
  if ArticleFound = '' then
  begin  
    i := Low(ETKArticles);
    while (ArticleFound = '') and (i <= High(ETKArticles)) do
    begin
      if AnsiStartsStr(ETKArticles[i] + ' ', aTitle) then
        ArticleFound := ETKArticles[i]
      else
        Inc(i);
    end;  
    if ArticleFound <> '' then
    begin
      SortTitle := Trim(ETKCopyFrom(aTitle, Length(ArticleFound) + 1)) + ', ' +
        ArticleFound;
      // Don't change aTitle
    end; 
  end; 
  
  // Really not needed now...
  if CompareText(aTitle, SortTitle) = 0 then
    SortTitle := '';
  
  // Setting MediaFile
  if SortTitle <> '' then
    MediaFile := CleanFileName(SortTitle, true, false)
  else    
    MediaFile := CleanFileName(aTitle, true, false);
end;
