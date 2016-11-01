module Printf

data Format = Number Format
            | Str Format
            | Chr Format
            | Doub Format
            | Lit String Format
            | End


PrintfType : Format -> Type
PrintfType (Number fmt)  = (i : Int) -> PrintfType fmt
PrintfType (Str fmt)     = (str : String) -> PrintfType fmt
-- `str` is ignored here because we are computing types, not values
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType (Chr fmt)     = (c : Char) -> PrintfType fmt
PrintfType (Doub fmt)    = (d : Double) -> PrintfType fmt
PrintfType End           = String

printFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printFmt (Number fmt) acc  = \i => printFmt fmt (acc ++ (show i))
printFmt (Str fmt) acc     = \s => printFmt fmt (acc ++ s)
printFmt (Lit str fmt) acc = printFmt fmt (acc ++ str)
printFmt (Chr fmt) acc     = \c => printFmt fmt (acc ++ (singleton c))
printFmt (Doub fmt) acc    = \d => printFmt fmt (acc ++ (show d))
printFmt End acc           = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number $ toFormat chars
toFormat ('%' :: 's' :: chars) = Str $ toFormat chars
toFormat ('%' :: 'c' :: chars) = Chr $ toFormat chars
toFormat ('%' :: 'f' :: chars) = Doub $ toFormat chars
toFormat ('%' :: chars)        = Lit "%" $ toFormat chars
-- Below implementation seems to be of an efficiency concern to me
-- If we didn't do strcons here, these literals would be concatenated by printFmt
toFormat (c :: chars)          = case toFormat chars of
                                     Lit lit chars' => Lit (strCons c lit) chars'
                                     fmt            => Lit (strCons c "") fmt

formatString : String -> Format
formatString = toFormat . unpack

printf : (fmt : String) -> PrintfType (formatString fmt)
printf fmt = printFmt _ ""

main : IO ()
main = do
     putStrLn (printf "%d" 1)
