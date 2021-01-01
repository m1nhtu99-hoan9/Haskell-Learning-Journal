module User where 

  newtype Username = Username String 
  newtype AccountNum = AccountNum Int 

  data User = UnregisteredUser | RegisteredUser Username AccountNum

  -- use pattern matching to expose internal data
  printUser :: User -> String
  printUser UnregisteredUser = "Unregistered User"
  printUser (RegisteredUser (Username username') (AccountNum num')) = 
    show $ concat ["Username is ", username', ", Account Number is ", show num']