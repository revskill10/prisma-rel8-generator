import { DMMF } from "@prisma/generator-helper";

export function mapPrismaToRel8(f: DMMF.Field) {
    switch (f.type) {
        case 'String':
            return f.isRequired ? 'Text' : '(Maybe Text)';
        case 'Int':
            return f.isRequired ? 'Int64': '(Maybe Int64)';
        case 'Boolean':
            return f.isRequired ? 'Bool': '(Maybe Bool)'
        case 'DateTime':
            return f.isRequired ? 'UTCTime' : '(Maybe UTCTime)'
        default:
            return 'Text';
    }
}