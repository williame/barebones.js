function QuadTree(rect) {
	assert(this instanceof QuadTree);
	this.rect = rect;
	this.centre = rect_centre(rect);
	this.child_nodes = null;
	this.children = [];
}
QuadTree.prototype = {
	split_threshold: 10,
	_getNode: function(rect) {
		if(rect_contains_vec2(rect,this.centre) || !this.child_nodes)
			return this;
		for(var i in this.child_nodes)
			if(rect_contains_rect(this.child_nodes[i].rect,rect))
				return this.child_nodes[i]._getNode(rect);
		return this;
	},
	getInRect: function(rect) {
		var list = [];
		this._getNode(rect)._getRect(rect,list);
		return list;
	},
	_getAll: function(list) {
		list.push.apply(list,this.children.
			map(function(child) { return child[0]; }));
		if(this.child_nodes)
			for(var i in this.child_nodes)
				this.child_nodes[i]._getAll(list);
	},
	_getRect: function(rect,list) {
		list.push.apply(list,this.children.
			filter(function(child) { return rect_intersects_rect(rect,child[1]); }).
			map(function(child) { return child[0]; }));
		if(this.child_nodes)
			for(var i in this.child_nodes) {
				var node = this.child_nodes[i];
				if(rect_contains_rect(node.rect,rect))
					node._getAll(list);
				else if(rect_intersects_rect(node.rect,rect))
					node._getRect(rect,list);
			}
	},
	add: function(rect,obj) {
		var node = this._getNode(rect);
		if(node.children.length > node.split_threshold && !node.child_nodes) {
			var half_size = rect_half_size(node.rect);
			var half_x = half_size[0], half_y = half_size[1];
			var ofs_x = node.rect[0], ofs_y = node.rect[1];
			half_size = [0,0,half_x,half_y];
			node.child_nodes = [
				new QuadTree(rect_add_vec2(half_size,[ofs_x,ofs_y])), // TL
				new QuadTree(rect_add_vec2(half_size,[ofs_x+half_x,ofs_y])), // TR
				new QuadTree(rect_add_vec2(half_size,[ofs_x,ofs_y+half_y])), // BL
				new QuadTree(rect_add_vec2(half_size,[ofs_x+half_x,ofs_y+half_y]))]; // BR
			var split = node.children;
			node.children = [];
			for(var i in split)
				node.add(split[i][1],split[i][0]);
			node.add(rect,obj);
		} else
			node.children.push([obj,rect]);
	},
	remove: function(rect,obj) {
		var node = this._getNode(rect);
		for(var i in node.children)
			if(node.children[i][0] == obj) {
				assert(rect == node.children[i][1]);
				node.children.splice(i,1);
				return;
			}
		fail("could not remove",rect,obj);
	},
};
