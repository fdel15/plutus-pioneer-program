{-# LANGUAGE OverloadedStrings #-}
import Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract "Charles" "Simon" "Alex" $ Constant 100

choiceId :: Party -> ChoiceId
choiceId p = ChoiceId "Winner" p

contract :: Party -> Party -> Party -> Value -> Contract
contract alice bob charlie deposit =
    When
        [ f alice bob charlie
        , f bob alice charlie
        ]
        10 Close
  where
    f :: Party -> Party -> Party -> Case
    f x y z =
        Case
            -- Action
            (Deposit
                x
                x
                ada
                deposit
            )
            -- Contract 1
            (When
                [Case
                    -- Action
                    (Deposit
                        y
                        y
                        ada
                        deposit
                    )
                    -- Contract 2
                    (When
                        [Case
                            -- Action
                            (Deposit
                                z
                                z
                                ada
                                (MulValue deposit (Constant 2))
                            )
                            -- Contract 3
                            (When
                                [Case
                                    -- Action
                                    (Choice
                                        (choiceId charlie)
                                        [Bound 1 2]
                                    )
                                    -- Contract 4
                                    (If
                                        (ValueEQ
                                            (ChoiceValue $ choiceId charlie)
                                            (Constant 1)
                                        )
                                            (Pay
                                                bob
                                                (Account alice)
                                                ada
                                                deposit
                                                Close -- Ends Contract 4
                                            )
                                            (Pay
                                                alice
                                                (Account bob)
                                                ada
                                                deposit
                                                Close -- Ends Contract 4
                                            )
                                    )]
                            30 (Pay
                                  charlie
                                  (Account bob)
                                  ada
                                  deposit
                                  (Pay
                                     charlie
                                     (Account alice)
                                     ada
                                     deposit
                                     Close
                                  )
                                ) -- Ends Contract 3
                            )]
                        20 Close -- Ends Contract 2
                    )]
                20 Close -- Ends Conract 1
            )
