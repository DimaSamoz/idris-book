||| Checks whether the input is a palindrome.
||| Case-insensitive and only returns True if the palindrome is more than len characters long.
||| @len the number of characters above which palindromes are checked.
||| @str the input string.
palindrome : (len : Nat) -> (str : String) -> Bool
palindrome len str = length lowercase > len && lowercase == reverse lowercase
    where
        lowercase : String
        lowercase = toLower str

||| Returns the number of words and characters in the input.
||| @str the input string.
counts : (str : String) -> (Nat, Nat)
counts str = (length (words str), length str)

||| Returns the 10 largest results in the list.
||| @list the input list.
top_ten : Ord a => (list : List a) -> (List a)
top_ten lst = take 10 (reverse (sort lst))

||| Returns the number of words in list longer than n.
||| @n the length limit.
||| @list the input list of strings.
over_length : (n : Nat) -> (list : List String) -> Nat
over_length n = length . filter (\str => length str > n)
