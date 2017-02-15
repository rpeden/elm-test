module Animals exposing (Animal (..), Being (..), Anything (..),checkName)

type Animal = Cat String Int | Dog String Int

type Being = Human String Int | Blob String Int

type Anything = Ani Animal | Bei Being

cat : Animal
cat = Cat "Norm" 21

dog : Animal
dog = Dog "Ted" 55

checkAnimalName : Animal -> String
checkAnimalName animal =
  case animal of
    Cat name age -> 
      "I'm a cat named " ++ name
    Dog name age ->
      "I'm a dog named " ++ name 

checkBeingName : Being -> String
checkBeingName being = 
  case being of 
    Human name age ->
      name
    Blob name age -> 
      name

checkName : Anything -> String
checkName thing = 
  case thing of
    Ani animal -> 
      checkAnimalName animal
    Bei being -> 
      checkBeingName being