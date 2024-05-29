//
// We should not export types and/or values from .d.ts files.
// .d.ts files are for ambient declarations and should not
// be treated as modules.
//

export type StorageItem = {
  weight: number;
};

export type ShipStorage = {
  max: number;
  items: StorageItem[];
};
