# Сайт конференции PLC-2017

## Зависимости

* GHC >=7.8,

* cabal >= 1.18,

* Hakyll 4.

## Сборка

Один раз:

    cabal build
    
При обновлении сайта:

1. Локальная перегенерация

        dist/build/site/site build
        
2. Развёртывание на сайт (по SSH, пока на `staff.mmcs/~ulysses/_site` — нужно быть Улиссом):

        dist/build/site/site deploy
