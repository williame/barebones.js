# a Python script for optimising Glest G3D models.
# To be used in combination with G3DHack.
# Often, G3D models contain lots of meshes that have the same textures and number of frames;
# this tool coalesces them.  This increases the possibilities for G3DHack optimisations
# and reduces the number of draw calls needed to draw the model in-game.

import struct, sys, os

class Reader:
    def __init__(self,bytes):
        self.bytes, self.ofs = bytes, 0
    def skip(self,len): self.ofs += len
    def readU8(self): return self._read("B",1)[0]
    def readU16(self): return self._read("<H",2)[0]
    def readU32(self): return self._read("<I",4)[0]
    def readF32(self,len): return self._read("<"+"f"*len,4*len)
    def readS64(self): return ("".join(self._read("c"*64,64))).split('\x00')[0]
    def read(self,fmt): return self._read(fmt,struct.calcsize(fmt))
    def _read(self,fmt,len):
        self.ofs += len
        return struct.unpack(fmt,self.bytes[self.ofs-len:self.ofs])

class G3D:

    class Mesh:
        
        def __init__(self,g3d,f):
            self.g3d = g3d
            self.name = f.readS64()
            self.frame_count, self.vertex_count, \
                self.index_count = f.read("<III")
            self.skip = f.read("b"*8*4)
            properties = f.readU32()
            self.customColour = properties&1
            self.twoSided = properties&2
            textures = f.readU32()
            self.texture = None 
            for t in range(5):
                if textures&(1<<t):
                    texture = f.readS64()
                    if not t:
                        self.texture = texture
            class Frame: pass
            self.frames = [Frame() for _ in range(self.frame_count)]
            for frame in self.frames:
                frame.vertices = list(f.readF32(self.vertex_count*3))
            for frame in self.frames:
                frame.normals = list(f.readF32(self.vertex_count*3))
            if textures:
                self.textures = list(f.readF32(self.vertex_count*2))
            self.indices = list(f.read("<"+"I"*self.index_count))
            
        def write(self,f):
            f.write(struct.pack("c"*64,*self.name.ljust(64,'\x00')))
            f.write(struct.pack("<III",self.frame_count,self.vertex_count,self.index_count))
            f.write(struct.pack("b"*8*4,*self.skip))
            f.write(struct.pack("<I",self.customColour|self.twoSided))
            f.write(struct.pack("<I",1 if self.texture else 0))
            if self.texture:
                f.write(struct.pack("c"*64,*self.texture.ljust(64,'\x00')))
            for frame in self.frames:
                f.write(struct.pack("<"+"f"*self.vertex_count*3,*frame.vertices))
            for frame in self.frames:
                f.write(struct.pack("<"+"f"*self.vertex_count*3,*frame.normals))
            if self.texture:
                f.write(struct.pack("<"+"f"*self.vertex_count*2,*self.textures))
            f.write(struct.pack("<"+"I"*self.index_count,*self.indices))

        def __repr__(self):
            return self.name
    
    def __init__(self,name,bytes):
        self.name = name
        self.meshes = []
        f = Reader(bytes)
        if f.read("cccb") != ('G','3','D',4):
            raise Exception("bad magic")
        mesh_count = f.readU16()
        if f.readU8():
            raise Exception("not an mtMorphMesh")
        for i in range(mesh_count):
            self.meshes.append(self.Mesh(self,f))
            
    def __repr__(self):
        return self.name
            
    def auto_join_frames(self):
        print "auto-joining compatible meshes..."
        meshes = {}
        for mesh in self.meshes:
            key = (mesh.texture,mesh.frame_count,mesh.twoSided|mesh.customColour)
            if key in meshes:
                meshes[key].append(mesh)
            else:
                meshes[key] = [mesh]
        for joinable in meshes.values():
            if len(joinable) < 2: continue
            base = joinable[0]
            print "\tjoining to",base
            for mesh in joinable[1:]:
                if base.index_count+mesh.index_count > 0xffff:
                    base = mesh
                    print "\tjoining to",base
                    continue
                print "\t\t",mesh
                for a,b in zip(base.frames,mesh.frames):
                    a.vertices.extend(b.vertices)
                    a.normals.extend(b.normals)
                if base.texture:
                    base.textures.extend(mesh.textures)
                base.indices.extend(index+base.vertex_count for index in mesh.indices)
                base.vertex_count += mesh.vertex_count
                base.index_count += mesh.index_count
                self.meshes.remove(mesh)

    def rename_texture(self,old,new):
        for mesh in self.meshes:
            if mesh.texture == old:
                print "renaming",old,"to",new,"in",mesh
                mesh.texture = new

    def write(self,f):
        f.write(struct.pack("cccb",'G','3','D',4))
        f.write(struct.pack("<H",len(self.meshes)))
        f.write(struct.pack("b",0))
        for mesh in self.meshes:
            mesh.write(f)

    def desc(self):
        print "G3D %s has %d meshes"%(self.name,len(self.meshes))
        for mesh in self.meshes:
            print "\t%s has %d frames, %d vertices and %d indices"% \
                (mesh.name,mesh.frame_count,mesh.vertex_count,mesh.index_count)
        
if __name__=="__main__":
    if len(sys.argv) != 3:
        sys.exit("usage: python g3dopt.py [src] [dest]")
    src = sys.argv[1]
    g3d = G3D(src,file(src,"rb").read())
    g3d.desc()
    g3d.auto_join_frames()
    g3d.desc()
    dest = sys.argv[2]
    print "saving",dest,"..."
    g3d.write(file(dest,"wb"))
