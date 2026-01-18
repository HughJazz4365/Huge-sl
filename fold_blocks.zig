
        if (s.* == .block and flatten_blocks) {
            const block = self.getBlock(s.block);
            const len = block.body.items.len;
            if (len == 0) {
                s.* = .empty;
                i += 1;
                continue;
            }
            try scope.body.ensureUnusedCapacity(self.arena.allocator(), len - 1);
            const movelen = scope.body.items.len - i - 1;
            scope.body.items.len += len - 1;
            @memmove(
                scope.body.items[i + len .. i + len + movelen],
                scope.body.items[i + 1 .. i + 1 + movelen],
            );
            @memcpy(
                scope.body.items[i .. i + len],
                block.body.items,
            );
            i += len;
        } else i += 1;
