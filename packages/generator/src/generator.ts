import { generatorHandler, GeneratorOptions } from '@prisma/generator-helper'
import { logger } from '@prisma/sdk'
import { GENERATOR_NAME } from './constants'
import { mapPrismaToRel8 } from './helpers/mapPrismaToRel8'
import { writeFileSafely } from './utils/writeFileSafely'
import path from 'path'
const decapitalizeTest = (str: String) => `${str.charAt(0).toLowerCase()}${str.slice(1)}`;
const capitalizeTest = (str: String) => `${str.charAt(0).toUpperCase()}${str.slice(1)}`;
const { version } = require('../package.json')
const toCamelCase = (str: String) =>
    str
        .replace(/[^a-zA-Z0-9]+(.)/g, (m, chr) => chr.toUpperCase())
        .replace(/^\w/, c => c.toLowerCase());
generatorHandler({
    onManifest() {
        logger.info(`${GENERATOR_NAME}:Registered`)
        return {
            version,
            defaultOutput: '../generated',
            prettyName: GENERATOR_NAME,
        }
    },
    onGenerate: async (options: GeneratorOptions) => {
        const models: string[] = []
        options.dmmf.datamodel.models.forEach(async (dataModel) => {

            const modelName = `${dataModel.name}Model`

            models.push(`newtype ${modelName}Id = ${modelName}Id { toInt64 :: Int64 }
    deriving newtype (DBEq, DBType, Eq, Show)
      `)



            const relationFields = dataModel.fields.map(f => {
                if (f.relationFromFields) {
                    return { [f.relationFromFields[0]]: f.type }
                } else {
                    return null;
                }
            }).filter(f => !!f).reduce((prev, cur) => {
                return {
                    ...prev,
                    ...cur
                }
            }, {})

            const fields = dataModel.fields.map(f => {
                if (f.relationFromFields) {
                    // console.log('relation', f.relationFromFields)
                    return null
                } else {
                    return `${decapitalizeTest(modelName)}${capitalizeTest(toCamelCase(f.name))} = "${f.name}"`
                }
            }).filter(x => !!x)

            const dataFields = dataModel.fields.map(f => {
                if (f.relationToFields) {
                    console.log('relation', f.relationFromFields)
                    return null
                } else {
                    if (f.isId) {
                        return `${decapitalizeTest(modelName)}Id :: Column f ${modelName}Id`
                    } else {
                        if (relationFields && Object.keys(relationFields).includes(f.name)) {
                            return `${decapitalizeTest(modelName)}${capitalizeTest(toCamelCase(f.name))}:: Column f ${relationFields[f.name]}ModelId`
                        } else {
                            return `${decapitalizeTest(modelName)}${capitalizeTest(toCamelCase(f.name))}:: Column f ${mapPrismaToRel8(f)}`
                        }
                    }
                }
            }).filter(x => !!x)

            models.push(`data ${modelName} f = ${modelName}
    { ${dataFields.join('\n     ,')}
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

${decapitalizeTest(modelName)}Schema :: TableSchema (${modelName} Name)
${decapitalizeTest(modelName)}Schema = TableSchema
  { name = "${dataModel.dbName}"
  , schema = Nothing
  , columns = ${modelName}
      { ${fields.join('\n       ,')}
      }
  }
      `)

            models.push(`deriving stock instance f ~ Result => Show (${modelName} f)`)
            console.log(models)
        })
        const tmpl = `
module Database.Models where

import Internal.Prelude
import Rel8

${models.join("\n\n")}
    `

        await writeFileSafely(path.join(options.generator.output?.value || '.'), tmpl)
    },
})
