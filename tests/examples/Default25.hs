yes = if nullPS s then return False else if headPS s /= '\n' then return False else alter_input tailPS >> return True 
   