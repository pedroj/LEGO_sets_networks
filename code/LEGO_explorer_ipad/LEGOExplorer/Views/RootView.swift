import SwiftUI

struct RootView: View {
    @Environment(AppModel.self) private var model
    @State private var columns = NavigationSplitViewVisibility.all

    var body: some View {
        @Bindable var model = model
        VStack(spacing: 0) {
            BannerView()
            Divider()
            NavigationSplitView(columnVisibility: $columns) {
                SidebarView()
                    .navigationSplitViewColumnWidth(min: 300, ideal: 330, max: 400)
            } content: {
                ResultsView()
                    .navigationSplitViewColumnWidth(min: 340, ideal: 430, max: 560)
            } detail: {
                DetailView()
            }
        }
        .sheet(isPresented: $model.showData) { DataView().pageSizedSheet() }
    }
}

/// Banner: icon + title on the left, Santa Ana Bricks logo and credit link on the right.
struct BannerView: View {
    var body: some View {
        HStack(spacing: 12) {
            Image("BannerIcon").resizable().scaledToFit().frame(height: 48)
                .clipShape(RoundedRectangle(cornerRadius: 8))
            Text("LEGO Sets & Parts Explorer").font(.title.weight(.semibold))
            Spacer()
            VStack(spacing: 2) {
                Link(destination: URL(string: "https://rebrickable.com/users/PedroJ/")!) {
                    Image("SantaAnaBricks").resizable().scaledToFit().frame(height: 52)
                        .clipShape(RoundedRectangle(cornerRadius: 3))
                }
                Link("Designed by PedroJ @ Santa Ana Bricks", destination: URL(string: "https://rebrickable.com/users/PedroJ/")!)
                    .font(.caption)
            }
        }
        .padding(.horizontal, 20)
        .padding(.vertical, 8)
        .background(Color(.secondarySystemBackground), ignoresSafeAreaEdges: .top)
    }
}

extension View {
    /// Full-size sheet on iPadOS 18+ (the default form sheet is too short for the update progress).
    @ViewBuilder func pageSizedSheet() -> some View {
        if #available(iOS 18.0, *) { self.presentationSizing(.page) } else { self }
    }
}
